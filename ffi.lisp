;;; $Id: packages.lisp 78 2006-04-02 15:48:54Z  $
;;;
;;; Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.
;;;
;;; FFI for SpookyDistance CommonLispReflection DLL.
;;;
;;; Function names starting with a percent sign take or return
;;; unwrapped CLR object handles. All others deal only with CLR object
;;; handles that are wrapped with instances of CLR-OBJECT.
;;;

(in-package :cl-clr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CFFI initialization
;;;

(defctype clr-handle :pointer)

(load-foreign-library
 '(:default "SpookyDistance.CommonLispReflection.DllInterface"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lisp wrapper class for CLR handle memory management
;;;
(defclass clr-object ()
  ((handle :accessor handle-of :initarg :handle
           :documentation "void* used in FFI calls"))
  (:documentation "Lisp object representing a Common Language Runtime object."))

;;; Note that we give the type of release-object-handle's argument as
;;; :pointer, not as clr-handle. That's so we don't inadvertently hand
;;; a CLR-OBJECT to %RELEASE-OBJECT-HANDLE -- see TRANSLATE-TO-FOEIGN,
;;; below.
(defcfun ("release_object_handle" %release-object-handle) :void
  (handle :pointer))

;;; TODO: need vendor-specific implementations
#+lispworks
(defun flag-for-handle-release (obj)
  (hcl:flag-special-free-action obj))

(defvar *live-clr-objects* 0)

(defun %make-clr-object (handle)
  (let ((result (make-instance 'clr-object :handle handle)))
    (flag-for-handle-release result)
    (incf *live-clr-objects*)
    result))

(defun clr-object-p (obj)
  "Returns true if OBJ is a CLR-OBJECT, otherwise NIL."
  (typep obj 'clr-object))

#+lispworks
(defun lispworks-free-clr-handle (obj)
  (when (typep obj 'clr-object)
    (decf *live-clr-objects*)
    (%release-object-handle (handle-of obj))
    (hcl:flag-not-special-free-action obj)))

#+lispworks
(hcl:add-special-free-action 'lispworks-free-clr-handle)
  
(defun release-if-naked-handle (value)
  "VALUE must be a CLR handle, NIL, or a CLR-OBJECT. In the
latter two cases, this function does nothing. In the first case,
it releases the handle. Returns nothing."
  (unless (or (eq nil value) (typep value 'clr-object))
    (%release-object-handle value))
  (values))

;;; Anywhere that we allow a handle, we also allow a CLR-OBJECT. Here
;;; we tell CFFI how to translate CLR-OBJECTs to handles at the last
;;; moment, just before a FFI call.
(defmethod translate-to-foreign (value (type (eql 'clr-handle)))
  (if (typep value 'clr-object)
      (handle-of value)
      value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exception signaling
;;;
;;; Exceptions are denoted by a special class of object returned
;;; from FFI calls. When one of these is encountered, we signal
;;; a CLR-EXCEPTION condition.
;;;
;;; A PRINT-OBJECT method for CLR-EXCEPTION instances is defined in
;;; invoke.lisp, where better tools exist for retrieving information
;;; from the exception object.
;;;
(define-condition clr-exception (error clr-object)
  ()
  (:documentation "A condition raised to indicate that an
 exception was thrown by CLR code invoked via CL-CLR."))

(defcfun ("returned_exception" %returned-exception) clr-handle
  (returned-value clr-handle))

(defun %signal-if-exception (handle)
  (let ((e (%returned-exception handle)))
    (unless (null-pointer-p e)
      (%release-object-handle handle)
      (error (make-condition 'clr-exception :handle e))))
  handle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Boxing and unboxing Lisp values
;;;

(defmacro define-boxing-functions (clr-name cffi-type
                                   &optional
                                   (boxname (concatenate 'string
                                                         "box_" clr-name))
                                   (unboxname (concatenate 'string
                                                           "unbox_" clr-name)))
  `(progn
     (defcfun (,boxname
               ,(read-from-string (concatenate 'string "%box-"
                                               (map 'string
                                                    #'char-downcase
                                                    clr-name))))
         clr-handle
       (value ,cffi-type))
     (defcfun (,unboxname
               ,(read-from-string (concatenate 'string "%unbox-"
                                               (map 'string
                                                    #'char-downcase
                                                    clr-name))))
         ,cffi-type
       (value clr-handle))))

(define-boxing-functions "Byte"   :uint8)
(define-boxing-functions "Int16"  :int16)
(define-boxing-functions "Int32"  :int32)
#-cffi-features:no-long-long
(define-boxing-functions "Int64"  :int64)
(define-boxing-functions "String" :string)
(define-boxing-functions "Double" :double)
#-:lispworks
(define-boxing-functions "Single" :single)
#+:lispworks
(define-boxing-functions "Single" :double
  "box_SingleFromDouble" "unbox_DoubleFromSingle")
(define-boxing-functions "Char"   :char)
(define-boxing-functions "Boolean" :boolean)

(defun %integer-to-handle (value)
  (cond
    ((< value (expt 2  8)) (%box-byte  value))
    ((< value (expt 2 15)) (%box-int16 value))
    ((< value (expt 2 31)) (%box-int32 value))
    #-cffi-features:no-long-long
    ((< value (expt 2 63)) (%box-int64 value))
    ((plusp value) (error "Integer value too large to box: ~A" value))
    ((>= value (- (expt 2 15))) (%box-int16 value))
    ((>= value (- (expt 2 31))) (%box-int32 value))
    #-cffi-features:no-long-long
    ((>= value (- (expt 2 63))) (%box-int64 value))
    (t (error "Integer value too small to box: ~A" value))))

(defun %value-to-handle (value)
  "Converts a Lisp value to a handle if it's a boxable
value. CLR-OBJECTs are returned unmodified, according to the
guideline that they can appear anywhere that a handle
can. Signals an error otherwise."
  (typecase value
    (clr-object   value)
    (integer      (%integer-to-handle value))
    (string       (%box-string value))
    (double-float (%box-double value))
    (single-float (%box-single value))
    (character    (%box-char value))
    (ratio        (%box-double (coerce value 'double-float)))
    (null         (make-pointer 0))
    (pathname     (%box-string (namestring value)))
    (otherwise    (if (eq value t)
                      (%box-boolean 1)
                      (error "Don't know how to convert ~S to a CLR-HANDLE."
                             value)))))

;;;
;;; Define symbols and predicates for all of the simple type code
;;; constants.
;;;
(defmacro def-simple-type-code (clr-name)
  (let ((const-name (read-from-string (concatenate 'string "*typecode-"
                                         (map 'string
                                              #'char-downcase
                                              clr-name)
                                         "*"))))
    `(defvar ,const-name (foreign-funcall "type_type_code"
                                          :string ,clr-name :int))))

;; TODO: This can be optimized using an array lookup. The typecodes
;; are not likely ever to change. The ones referenced here range fom 3
;; to 18.
(def-simple-type-code "Byte")
(def-simple-type-code "Int16")
(def-simple-type-code "Int32")
#-cffi-features:no-long-long
(def-simple-type-code "Int64")
(def-simple-type-code "String")
(def-simple-type-code "Double")
(def-simple-type-code "Single")
(def-simple-type-code "Char")
(def-simple-type-code "Boolean")

(defun %handle-to-value (handle)
  "Converts a CLR handle to a value, freeing the handle if this
occurs by unboxing a value. Otherwise, it returns the same
handle, wrapped as a CLR-OBJECT. Returns nothing if handle refers
to the void-return object. Signals a CLR exception condition if
handle refers to an exception-return object."
  (when (null-pointer-p handle)
    (return-from %handle-to-value nil))
  (let* ((code (%object-type-code handle))
         (result
          (cond
            ((eql code *typecode-byte*) (%unbox-byte handle))
            ((eql code *typecode-int16*) (%unbox-int16 handle))
            ((eql code *typecode-int32*) (%unbox-int32 handle))
            #-cffi-features:no-long-long
            ((eql code *typecode-int64*) (%unbox-int64 handle))
            ((eql code *typecode-string*) (%unbox-string handle))
            ((eql code *typecode-char*) (%unbox-char handle))
            ((eql code *typecode-double*) (%unbox-double handle))
            ((eql code *typecode-single*) (%unbox-single handle))
            ((eql code *typecode-boolean*) (%unbox-boolean handle))
            (t
             (%signal-if-exception handle)
             (when (%is-void-return handle)
               (%release-object-handle handle)
               (return-from %handle-to-value (values)))
             (return-from %handle-to-value (%make-clr-object handle))))))
    (%release-object-handle handle)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Other FFI functions
;;;
;;; We wrap all FFI functions, except the very primitive ones defined
;;; earlier in this file, with Lisp code to box arguments and to test
;;; return values for exception indicators.
;;;

(defun arg-to-handle (arg temp-stack)
  (cond
    ((typep arg 'clr-object) arg)
    (t (let ((handle (%value-to-handle arg)))
         (vector-push handle temp-stack)
         handle))))

;; TODO: This is probably a bad idea, because sometimes we want to
;; handle straight, unwrapped handles in, and when calling general FFI
;; functions we can't tell the difference between them and any other
;; pointer. Although ARG-TO-HANDLE, above, probably *will* be useful.

(defmacro define-ffi-function (name return-type &rest args)
  "Using CFFI, define a foreign function that takes and/or
returns CLR handles, and a companion Lisp function that takes
care of boxing and unboxing the arguments."
  (let ((c-name (first name))
        (cffi-name (second name))
        (lisp-name (third name))
        
        (arg-names (map 'list #'first args))
        (handle-names (loop
                         for arg in args
                         when (eq (second arg) 'clr-handle)
                         collect (first arg)))
        (temp-stack (gensym)))
                           
    `(progn
       (defcfun (,c-name ,cffi-name) ,return-type ,@args)
       (defun ,lisp-name ,arg-names
         (let ((,temp-stack
                (make-array ,(length handle-names)
                            :fill-pointer 0)))
           (declare (dynamic-extent ,temp-stack))
           (unwind-protect
                (progn
                  ,@(map 'list
                         (lambda (name)
                           `(setf ,name (arg-to-handle ,name ,temp-stack)))
                         handle-names)
                  ,(if (eq return-type 'clr-handle)
                       `(%signal-if-exception (,cffi-name ,@arg-names))
                       `(,cffi-name ,@arg-names)))
             (loop
                while (plusp (fill-pointer ,temp-stack))
                do (%release-object-handle (vector-pop ,temp-stack)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Other FFI functions
;;;

(defctype clr-handle :pointer)

(load-foreign-library
 '(:default "SpookyDistance.CommonLispReflection.DllInterface"))

(defcfun "number_of_unreleased_handles" :int)

(defcfun ("invoke_member" %invoke-member) clr-handle
  (type-handle clr-handle)
  (name :string)
  (binding-flags :int)
  (binder-handle clr-handle)
  (object-handle clr-handle)
  (args-handle clr-handle))

(defcfun ("is_void_return" %is-void-return) :boolean
  (returned-value clr-handle))

(defcfun ("get_system_type" %get-system-type) clr-handle
  (name :string))

(defcfun ("make_array" %make-array) clr-handle
  (n :int)
  (element-type clr-handle))

(defcfun ("get_array_element" %get-array-element) clr-handle
  (array clr-handle)
  (index :int))

(defcfun ("set_array_element" %set-array-element) clr-handle
  (array clr-handle)
  (index :int)
  (obj clr-handle))

(defcfun ("wrap_varargs_array" %wrap-varargs-array) clr-handle
  (handle clr-handle))

(defcfun ("type_type_code" type-type-code) :int
    (type-name   :string))

(defcfun ("object_type_code" %object-type-code) :int
    (object clr-handle))

(defcfun ("enum_value" %enum-value) clr-handle
  (type clr-handle)
  (name :string))

(defcfun ("make_callback_delegate" %make-callback-delegate) clr-handle
  (identifier :int)
  (foreign_callback :pointer)
  (release_callback :pointer)
  (delegate-type clr-handle))

(defcfun ("invoke_delegate" %invoke-delegate) clr-handle
  (delegate-handle clr-handle)
  (args clr-handle))

(defcfun ("make_lisp_binder" %make-lisp-binder) clr-handle
  (allow-double-narrowing :boolean))

(defmacro %with-clr-handle ((var init) &body body)
  (assert (symbolp var))
  `(let ((,var ,init))
     (unwind-protect
          (progn ,@body)
       (release-if-naked-handle ,var))))

(defmacro %with-clr-handles* ((&rest var-forms) &body body)
  (if var-forms
      `(%with-clr-handle ,(car var-forms)
         (%with-clr-handles* ,(cdr var-forms)
           ,@body))
      `(progn ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Binding flags -- needed for most invocations, so we provide
;;; a convenience function. It's similar to OR-ENUM-VALUES.
;;;
(defun binding-flags (&rest args)
  (%with-clr-handle (type (%get-system-type "System.Reflection.BindingFlags"))
    (reduce #'(lambda (value arg)
                (logior value
                        (etypecase arg
                          (string (%handle-to-value (%enum-value type arg)))
                          (integer arg))))
            args :initial-value 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Callback support
;;;

(defun bad-callback (&optional args)
  (declare (ignore args))
  (error "Callback occurred from CLR to a bad callback index."))

(defvar *callback-array*
  (make-array 10
              :element-type 'function
              :initial-element #'bad-callback
              :adjustable t
              :fill-pointer 0))

(defun %register-callback (fun)
  "Registers a callback function, which must be a function taking
an integer argument count and a CLR handle of type
System.Array. Returns an index that can be used to retrieve or
unregister the callback."
  (let ((index  (loop
                   for i from 0 below (fill-pointer *callback-array*)
                   when (not (eq (aref *callback-array* i) #'bad-callback))
                   return i)))
    (cond
      (index (setf (aref *callback-array* index) fun) index)
      (t (vector-push-extend fun *callback-array*)))))

(defun register-clr-callback (fun)
  "Registers a callback function. Returns an index that can be
used to retrieve or unregister the callback."
  (%register-callback
   #'(lambda (n-args args-array)
       (apply fun
              (loop
                 for i from 0 below n-args
                 collecting (%handle-to-value
                             (%get-array-element args-array i)))))))

(defcallback %callback-proxy clr-handle
    ((index :int) (n-args :int) (args-handle clr-handle))
  "A callback function proxy. Index identifies the actual Lisp
function to call. We assume that the lisp function can take its
arguments as a single naked CLR handle of a System.Array. This
allows for efficient internal callbacks."
  (when (>= index (fill-pointer *callback-array*))
    (bad-callback))
  (%value-to-handle (funcall (aref *callback-array* index)
                             n-args
                             args-handle)))

(defcallback %release-callback :void
    ((index :int))
  (setf (aref *callback-array* index) #'bad-callback)
  (values))

(defun make-delegate (fun type)
  (%handle-to-value
   (%make-callback-delegate (register-clr-callback fun)
                            (callback %callback-proxy)
                            (callback %release-callback)
                            type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellany

(defun unreleased-handle-count ()
  (number-of-unreleased-handles))

(defun uncollected-object-count ()
  *live-clr-objects*)

(defun unwrapped-handle-count ()
    (- (unreleased-handle-count) (uncollected-object-count)))

(defun make-lisp-binder ()
  (%handle-to-value (%make-lisp-binder
                     #+(and :lispworks :win32) t
                     #-(and :lispworks :win32) nil)))

