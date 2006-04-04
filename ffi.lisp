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

(defctype clr-handle :pointer)

(define-foreign-library clr-reflection
  (t (:default "CommonLispReflection")))

(load-foreign-library 'clr-reflection)

(defcfun ("invoke_member" %invoke-member) clr-handle
  (type-handle clr-handle)
  (name :string)
  (binding-flags :int)
  (binder-handle clr-handle)
  (object-handle clr-handle)
  (args-handle clr-handle))

(defcfun ("returned_exception" %returned-exception) clr-handle
  (returned-value clr-handle))

(defcfun ("is_void_return" %is-void-return) :boolean
  (returned-value clr-handle))

(defcfun ("get_default_app_domain" %get-default-app-domain) clr-handle)

(defcfun ("get_system_type" %get-system-type) clr-handle
  (name :string))

(defcfun ("make_lisp_binder" %make-lisp-binder) clr-handle
  (allow-double-narrowing :boolean))

(defcfun ("make_object_array" %make-object-array) clr-handle
  (n :int))

(defcfun ("get_array_element" %get-array-element) clr-handle
  (array clr-handle)
  (index :int))

(defcfun ("set_array_element" %set-array-element) :void
  (array clr-handle)
  (index :int)
  (obj clr-handle))

(defcfun ("array_length" %array-length) :int
  (array clr-handle))

(defcfun ("release_object_handle" %release-object-handle) :void
  (handle clr-handle))

(defcfun ("is_simple_type" %is-simple-type) :boolean
    (object clr-handle)
    (type   :string))

(defcfun binding-flag :int
  (name :string))

(defun binding-flags (&rest name)
  (reduce #'(lambda (value name)
              (logior value (binding-flag name)))
          name :initial-value 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Memory management
;;;
#+lispworks
(hcl:add-special-free-action
 #'(lambda (obj)
     (when (typep obj 'clr-object)
       (%release-object-handle (handle-of obj))
       (hcl:flag-not-special-free-action obj))))

;;; TODO: need vendor implementations of flag-for-handle-release

#+lispworks
(defun flag-for-handle-release (obj)
  (hcl:flag-special-free-action obj))

(defmacro %with-clr-handle ((var init) &body body)
  (assert (symbolp var))
  `(let ((,var ,init))
     (unwind-protect
          (progn ,@body)
       (when ,var (%release-object-handle ,var)))))

(defmacro %with-clr-handles* ((&rest var-forms) &body body)
  (if var-forms
      `(%with-clr-handle ,(car var-forms)
         (%with-clr-handles* ,(cdr var-forms)
           ,@body))
      `(progn ,@body)))

(defun %make-args-array (args length)
  (let ((array (%make-object-array length)))
    (loop
       for arg in args
       for i upfrom 0  
       do (cond
            ((typep arg 'clr-object)
             (%set-array-element array i arg))
          (t (%with-clr-handle (handle (%box arg))
               (%set-array-element array i handle)))))
    array))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Boxing and unboxing
;;;

(defmacro define-boxing-functions (clr-name cffi-name &optional
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
       (value ,cffi-name))
     (defcfun (,unboxname
               ,(read-from-string (concatenate 'string "%unbox-"
                                               (map 'string
                                                    #'char-downcase
                                                    clr-name))))
         ,cffi-name
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

(defclass clr-object ()
  ((handle :accessor handle-of :initarg :handle)))

(defun %make-clr-object (handle)
  (let ((result (make-instance 'clr-object :handle handle)))
    (flag-for-handle-release result)
    result))

(defun clr-object-p (obj)
  (typep obj 'clr-object))

;;; Note: We don't define a corresponding translate-from-foreign,
;;; because we don't always want to automatically wrap a handle
;;; in a class.
(defmethod translate-to-foreign ((value clr-object) (type (eql 'clr-handle)))
  (handle-of value))

(defmethod box-lisp-value (value)
  (error "Don't know how to box ~S for CLR call." value))


(defun %box-integer (value)
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
  
(defun %box (value)
  (typecase value
    (clr-object   value)
    (integer      (%box-integer value))
    (string       (%box-string value))
    (double-float (%box-double value))
    (single-float (%box-single value))
    (character    (%box-char value))
    (ratio        (%box-double (coerce value 'double-float)))
    (null         (make-pointer 0))
    (pathname     (%box-string (namestring value)))
    (otherwise    (if (eq value t)
                      (%box-boolean 1)
                      (box-lisp-value value)))))

(defun %unbox (handle)
  (cond
    ((and (pointerp handle) (null-pointer-p handle)) nil)
    ((%is-simple-type handle "System.Byte")   (%unbox-byte  handle))
    ((%is-simple-type handle "System.Int16")  (%unbox-int16 handle))
    ((%is-simple-type handle "System.Int32")  (%unbox-int32 handle))
    #-cffi-features:no-long-long
    ((%is-simple-type handle "System.Int64")  (%unbox-int64 handle))
    ((%is-simple-type handle "System.String") (%unbox-string handle))
    ((%is-simple-type handle "System.Char")   (%unbox-char handle))
    ((%is-simple-type handle "System.Double") (%unbox-double handle))
    ((%is-simple-type handle "System.Single") (%unbox-single handle))
    ((%is-simple-type handle "System.Boolean") (%unbox-boolean handle))
    (t (%make-clr-object handle))))
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The root of most invocation evils:
;;;

(defun generic-invoke-member (type member-name flags object args
                              &key (binder *lisp-binder*))
  "A generic member invocation function based on
System.Type.InvokeMember. Works with the CommonLispReflection DLL
to manage the translation of void return types and thrown
exceptions. TYPE, OBJECT, and BINDER can be NIL and will be
boxed.  TYPE and OBJECT must not both be NIL. ARGS is expected to
be an unboxed handle of a CLR array, or NIL."
  ;; Be careful here. One of the values that can be returned by
  ;; %invoke-member is a singleton handle to denote the void
  ;; return. This value must not be boxed, otherwise we'll release it,
  ;; invalidating the handle. Besides, it would be a waste of
  ;; resources. Actually, similar comments apply to an exception
  ;; return handle -- no point in wrapping it as a CLR-OBJECT, since
  ;; we're only interested in the exception that it contains.
  (let* ((result (%invoke-member (%box type)
                                 member-name
                                 flags
                                 (%box binder)
                                 (%box object)
                                 (or args (make-pointer 0))))
         (e (%unbox (%returned-exception result))))
    (when e
      (%release-object-handle result)
      (error "Exception thrown."))
    (when (%is-void-return result)
      (return-from generic-invoke-member (values)))
    (%unbox result)))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Bootstrapping stuff for other modules.
;;;

(defun get-system-type (name)
  (%make-clr-object (%get-system-type name)))

(defmacro do-clr-array ((var array-form &optional result) &body body)
  "ARRAY-FORM should be a form which evaluates to a CLR object of
type ARRAY with a rank of one.  BODY will be evaluated with VAR
bound to each element of this array in turn.  Finally, the result
of evaluating the form RESULT is returned."
  (let ((array (gensym)))
    `(let ((,array ,array-form))
       (loop
          for i from 0 below (%array-length ,array)
          for ,var = (%unbox (%get-array-element ,array i))
          do (progn ,@body)
          finally return ,result))))

(defun clr-array-to-list (array)
  "Converts a CLR array ARRAY of rank 1 to a Lisp list with the
same elements."
  (loop
     for i from 0 below (%array-length array)
     collecting (%unbox (%get-array-element array i))))

(defun aref* (array index)
  (%unbox (%get-array-element array index)))

(defun (setf aref*) (array index value)
  (%unbox (%set-array-element array index (%box value))))

(defvar *default-app-domain* nil
  "The default application domain object. This is the root of all
type lookups.")
  
(defvar *lisp-binder* nil
  "The LispBinder object, a CLR object derived from System.Binder
that determines how members are selected.")

(defvar *system-type-type* nil
  "The type object representing System.Type. Used to bootstrap
retrieval of simple system types.")

(defvar *system-convert-type* nil
  "The type object for System.Convert.")

(defun init-ffi ()
  (setf
   *default-app-domain*  (%make-clr-object (%get-default-app-domain))
   *lisp-binder*         (%make-clr-object (%make-lisp-binder
                                            #+(and :lispworks :win32) t
                                            #-(and :lispworks :win32) nil))
   *system-convert-type* (get-system-type "System.Convert"))
  (values))

(defun shutdown-ffi ()
  (setf
   *system-convert-type* nil
   *default-app-domain*  nil
   *lisp-binder*         nil)
  (values))
