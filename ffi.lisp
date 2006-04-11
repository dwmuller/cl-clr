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
;;; DLL interface definition via CFFI.
;;;

(defctype clr-handle :pointer)

(load-foreign-library
 '(:default "SpookyDistance.CommonLispReflection.DllInterface"))

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

(defcfun ("release_object_handle" %release-object-handle) :void
  (handle clr-handle))

(defcfun ("wrap_varargs_array" %wrap-varargs-array) :pointer
  (handle clr-handle))

(defcfun ("type_type_code" type-type-code) :int
    (type-name   :string))

(defcfun ("object_type_code" %object-type-code) :int
    (object clr-handle))

(defcfun ("enum_value" %enum-value) clr-handle
  (type clr-handle)
  (name :string))

(defcfun ("make_lisp_binder" %make-lisp-binder) clr-handle
  (allow-double-narrowing :boolean))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exception handling
;;;
;;; A PRINT-OBJECT method for CLR-EXCEPTION instances is defined in
;;; invoke.lisp, where better tools exist for retrieving information
;;; from the exception object.
;;;
(define-condition clr-exception (error)
  ((exception :accessor exception-of :initarg :exception :type clr-object))
  (:documentation "A condition raised to indicate that an
 exception was thrown by CLR code invoked via CL-CLR."))

(defun %signal-if-exception (handle)
  (let ((e (%handle-to-value (%returned-exception handle))))
    (when e
      (%release-object-handle handle)
      (error (make-condition 'clr-exception :exception e))))
  handle)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Binding flags -- needed for most invocations, so we provide
;;; a convenience function. It's similar to OR-ENUM-VALUES.
;;;
(defun binding-flags (&rest args)
  (let ((type (%handle-to-value
               (%signal-if-exception
                (%get-system-type "System.Reflection.BindingFlags")))))
  (reduce #'(lambda (value arg)
              (logior value
                      (etypecase arg
                        (string (%handle-to-value
                                 (%signal-if-exception (%enum-value type arg))))
                        (integer arg))))
          args :initial-value 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Boxing and unboxing
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

(defclass clr-object ()
  ((handle :accessor handle-of :initarg :handle
           :documentation "void* used in FFI calls"))
  (:documentation "Lisp object representing a Common Language Runtime object."))

(defun %make-clr-object (handle)
  (let ((result (make-instance 'clr-object :handle handle)))
    (flag-for-handle-release result)
    result))

(defun clr-object-p (obj)
  "Returns true if OBJ is a CLR-OBJECT, otherwise NIL."
  (typep obj 'clr-object))

;;; Note: We don't define a corresponding translate-from-foreign,
;;; because we don't always want to automatically wrap a handle
;;; in a class.
(defmethod translate-to-foreign ((value clr-object) (type (eql 'clr-handle)))
  (handle-of value))

(defmethod lisp-value-to-clr-handle (value)
  (error "Don't know how to convert ~S to a CLR-HANDLE." value))


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
                      (%value-to-handle (lisp-value-to-clr-handle value))))))

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
  (unless (and (pointerp handle) (null-pointer-p handle))
    (let ((code (%object-type-code handle)))
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
        (t (%make-clr-object handle))))))
     
(defun make-lisp-binder ()
  (%make-clr-object (%signal-if-exception (%make-lisp-binder
                                           #+(and :lispworks :win32) t
                                           #-(and :lispworks :win32) nil))))

