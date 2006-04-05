;;; $Id: packages.lisp 78 2006-04-02 15:48:54Z  $
;;;
;;; Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.
;;;
;;; Low-level member invocation functions for CL-CLR.
;;;
;;; This file is loaded early, and should thus avoid using
;;; higher-level functionality. In particular, the use of type lookup
;;; (other than the system type lookup provided by ffi.lisp) and
;;; symbols-as-types functionality should be avoided here.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-clr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commonly used binding flag combinations.

(defvar *static-member-binding-flags*
  (binding-flags "Static"
                 "GetProperty"
                 "GetField"
                 "InvokeMethod"
                 "Public"))

(defvar *static-set-prop-or-field-binding-flags*
  (binding-flags "Static"
                 "SetProperty"
                 "SetField"
                 "Public"))

(defvar *instance-member-binding-flags*
  (binding-flags "Instance"
                 "GetProperty"
                 "GetField"
                 "InvokeMethod"
                 "Public"))
(defvar *instance-set-prop-or-field-binding-flags*
  (binding-flags "Instance"
                 "SetProperty"
                 "SetField"
                 "Public"))
(defvar *create-instance-binding-flags*
  (binding-flags "Instance"
                 "CreateInstance"
                 "Public"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CLR type objects used internally
;;
(defvar *system-object-type*
  "The CLR type object representing System.Object.")

(defvar *system-type-type* nil
  "The CLR type object representing System.Type.")

(defvar *system-array-type* nil
  "The CLR type object representing System.Array.")
  
(defvar *system-convert-type* nil
  "The CLR type object representing System.Convert.")

(defvar *system-enum-type* nil
  "The CLR type object representing System.Enum.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CLR objects used internally
;;
(defvar *lisp-binder* nil
  "The LispBinder object, a CLR object derived from System.Binder
that determines how members are selected.")

(defvar *default-app-domain* nil
  "The default application domain object. This is the root of all
type lookups.")
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
(defclass clr-ref ()
  ((value :initarg :value
          :accessor value-of
          :documentation "Input value for a REF or OUT argument.")
   (setter    :initarg :setter
              :accessor setter-of
              :documentation "A closure to take the new value
and deposit it in its destination.")))

(defmacro ref (form)
  "Marks an argument of a call to a CLR method as
by-reference. FORM must be a setf-able form. Returns an object of
a type internal to CL-CLR."
  (error "Not implemented yet."))

(defun copy-ref-args (args-list args-array)
  (loop
     for arg in args-list
     for i from 0
     do
       (when (typep arg 'clr-ref)
         (funcall (setter-of arg) (unbox (aref* args-array i)))))
  (values))
|#

(defun box (value)
  "Converts VALUE to a CLR-OBJECT, or signals an error if there
is no way to do this. If VALUE is already a CLR-OBJECT, it is
returned."
  (if (clr-object-p value)
      value
      (%make-clr-object (%value-to-handle value))))

(defun unbox (object)
  "Converts OBJECT from a CLR-OBJECT to a Lisp value. If OBJECT
is not a CLR-OBJECT, it is simply returned."
  (if (clr-object-p object)
      (%handle-to-value (handle-of object))
      object))

  
(defun get-system-type (name)
  "Get a CLR type object from its full name. This only works
reliably for built-in runtime system types.."
  (%make-clr-object (%signal-if-exception (%get-system-type name))))

(defmacro do-clr-array ((var init-form &optional result) &body body)
  "INIT-FORM should evaluate to a CLR object of type ARRAY with a
rank of one.  BODY will be evaluated with VAR bound to each
element of this array in turn.  Finally, the result of evaluating
the form RESULT is returned."
  (let ((array (gensym)))
    `(let ((,array ,init-form))
       (check-type ,array clr-object)
       (loop
          for i from 0 below (invoke-instance ,array "Length")
          for ,var = (%handle-to-value
                      (%signal-if-exception (%get-array-element ,array i)))
          do (progn ,@body)
          finally return ,result))))

(defun clr-array-to-list (array)
  "Converts a CLR array ARRAY of rank 1 to a Lisp list with the
same elements."
  (check-type array clr-object)
  (loop
     for i from 0 below (invoke-instance array "Length")
     collecting (%handle-to-value
                 (%signal-if-exception (%get-array-element array i)))))

(defun aref* (array index)
  "Return the element at offset INDEX in the CLR array object ARRAY."
  (check-type array clr-object)
  (check-type index integer)
  (%handle-to-value (%signal-if-exception (%get-array-element array index))))

(defun (setf aref*) (array index value)
  (check-type array clr-object)
  (check-type index integer)
  (%signal-if-exception (%set-array-element array
                                            index
                                            (%value-to-handle value)))
  value)

(defun invoke-static (type member-name &rest args)
  "Invokes or retrieves the value of the static member designated
by MEMBER-NAME and TYPE. TYPE must be a CLR-OBJECT referring to
an object derived from System.Type.  If the member is a property,
the ARGS are its indexes. If the member is a method, ARGS are the
arguments. If the member is a field, there must be no optional
arguments."
  (%with-clr-handle (args-array (%make-args-array args (length args)))
    (%generic-invoke-member type
                            member-name
                            *static-member-binding-flags*
                            nil
                            args-array)))

(defun invoke-instance (object member-name &rest args)
  "Invokes or retrieves the value of the instance member
designated by MEMBER-NAME and OBJECT. OBJECT must be a
CLR-OBJECT.  If the member is a property, the ARGS are its
indexes. If the member is a method, ARGS are the arguments. If
the member is a field, there must be no optional arguments."
  (%with-clr-handle (args-array (%make-args-array args (length args)))
    (%generic-invoke-member nil
                            member-name
                            *instance-member-binding-flags*
                            object
                            args-array)))

(defun set-static (new-value type member-name &rest indexes)
  "Sets the value of the member designated by MEMBER-NAME and
TYPE to NEW-VALUE, and returns NEW-VALUE. TYPE must be a
CLR-OBJECT referring to an object derived from System.Type.  If
the member is a property, the INDEXES are its indexes. If the
member is a field, there must be no optional arguments."
  (%with-clr-handles* ((args-array (%make-args-array indexes
                                                     (1+ (length indexes))))
                       (new-handle (%value-to-handle new-value)))
    ;; Tack the new value on the end of the args array.
    (%signal-if-exception (%set-array-element args-array
                                              (length indexes)
                                              new-handle))
    (%generic-invoke-member type
                            member-name
                            *static-set-prop-or-field-binding-flags*
                            nil
                            args-array))
  new-value)

(defun set-instance (new-value object member-name &rest indexes)
  "Sets the value of the member designated by MEMBER-NAME and
OBJECT to NEW-VALUE, and returns NEW-VALUE. OBJECT must be a
CLR-OBJECT.  If the member is a property, the INDEXES are its
indexes. If the member is a field, there must be no optional
arguments."
  (%with-clr-handles* ((args-array (%make-args-array indexes
                                                     (1+ (length indexes))))
                       (new-handle (%value-to-handle new-value)))
    ;; Tack the new value on the end of the args array.
    (%signal-if-exception (%set-array-element args-array
                                              (length indexes)
                                              new-handle))
    (%generic-invoke-member nil
                            member-name
                            *instance-set-prop-or-field-binding-flags*
                            object
                            args-array))
  new-value)

(defun invoke-new (type &rest args)
  "Make a new object of type indicated by TYPE, which must be a
symbol designating a CLR type, a CLR type object, or a
namespace-qualified name string of a CLR type."
  (%with-clr-handle (args-array (%make-args-array args (length args)))
    (%generic-invoke-member type
                            ".ctor"
                            *create-instance-binding-flags*
                            nil
                            args-array)))

#|
(defmacro define-clr-call (lisp-name
                           (&key clr-name
                                 type-name
                                 member-kind
                                 doc-string)
                           args)
  `(define-rdnzl-call
       ,lisp-name
       (,@(when clr-name (list :dotnet-name clr-name))
        ,@(when type-name (list :type-name
                                (if (symbolp type-name)
                                    (property (get-type-object type-name)
                                              "AssemblyQualifiedName")
                                    type-name)))
        ,@(when member-kind (list :member-kind member-kind))
        ,@(when doc-string (list :doc-string doc-string)))
     ,(map 'list #'(lambda (arg-spec)
                     (if (symbolp (second arg-spec))
                         (list (first arg-spec)
                               (property (get-type-object (second arg-spec))
                                         "AssemblyQualifiedName"))
                         arg-spec))
           args)))
|#

(defun init-invoke ()
  (setf
   *system-object-type*  (get-system-type "System.Object")
   *system-type-type*    (get-system-type "System.Type")
   *system-array-type*   (get-system-type "System.Array")
   *system-convert-type* (get-system-type "System.Convert")
   *system-enum-type*    (get-system-type "System.Enum")
   *lisp-binder*         (make-lisp-binder)
   *default-app-domain*  (invoke-static (get-system-type "System.AppDomain")
                                        "CurrentDomain"))
  (values))

(defun shutdown-invoke ()
  (setf
   *default-app-domain*  nil
   *lisp-binder*         nil
   *system-enum-type*    nil
   *system-convert-type* nil
   *system-array-type*   nil
   *system-type-type*    nil
   *system-object-type*  nil)
  (values))


