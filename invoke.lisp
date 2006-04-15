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
                 "Public"
                 "FlattenHierarchy"))
(defvar *static-set-prop-or-field-binding-flags*
  (binding-flags "Static"
                 "SetProperty"
                 "SetField"
                 "Public"
                 "FlattenHierarchy"))
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
;;;
;;; Basic array reading functions
;;;

(defun aref* (array index)
  "Return the element at offset INDEX in the CLR array object ARRAY."
  (check-type array clr-object)
  (check-type index integer)
  (%handle-to-value (%get-array-element array index)))

(defun (setf aref*) (value array index)
  (check-type array clr-object)
  (check-type index integer)
  (%with-clr-handle (value-handle (%value-to-handle value))
    (%handle-to-value (%set-array-element array index value-handle)))
  value)

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
          for ,var = (%handle-to-value (%get-array-element ,array i))
          do (progn ,@body)
          finally return ,result))))

(defun clr-array-to-list (array)
  "Converts a CLR array ARRAY of rank 1 to a Lisp list with the
same elements."
  (check-type array clr-object)
  (loop
     for i from 0 below (invoke-instance array "Length")
     collecting (%handle-to-value (%get-array-element array i))))

(defun get-system-type (name)
  "Get a CLR type object from its full name. This only works
reliably for built-in runtime system types.."
  (%handle-to-value (%get-system-type name)))

(defun %make-args-array (args &optional (size (length args)))
  "Creates a System.Array of rank 1 with element type
System.Object, copies the elements of the list ARGS into the
beginning of the array, and returns the array's handle. The array
length is SIZE, which defaults to (LENGTH ARGS) and must be at
least that large. This is used to build argument lists for
invocations."
  (%with-clr-handle (array-handle (%signal-if-exception
                                   (%make-array size *system-object-type*)))
    (loop
       for arg in args
       for i upfrom 0  
       do (%handle-to-value
           (cond
             ((typep arg 'clr-object)
              (%set-array-element array-handle i arg))
             (t (%with-clr-handle (handle (%value-to-handle arg))
                  (%set-array-element array-handle i handle))))))
    (let ((result array-handle))
      (setf array-handle nil)
      result)))

(defun invoke-static (type member-name &rest args)
  "Invokes or retrieves the value of the static member designated
by MEMBER-NAME and TYPE. TYPE must be a CLR-OBJECT referring to
an object derived from System.Type.  If the member is a property,
the ARGS are its indexes. If the member is a method, ARGS are the
arguments. If the member is a field, there must be no optional
arguments."
  (%with-clr-handle (args-array (%make-args-array args))
    (let ((result-handle (%invoke-member type
                                         member-name
                                         *static-member-binding-flags*
                                         *lisp-binder*
                                         nil
                                         args-array)))
      (%handle-to-value result-handle))))

(defun invoke-instance (object member-name &rest args)
  "Invokes or retrieves the value of the instance member
designated by MEMBER-NAME and OBJECT. OBJECT must be a
CLR-OBJECT.  If the member is a property, the ARGS are its
indexes. If the member is a method, ARGS are the arguments. If
the member is a field, there must be no optional arguments."
  (%with-clr-handle (args-array (%make-args-array args))
    (let ((result-handle (%invoke-member nil
                                         member-name
                                         *instance-member-binding-flags*
                                         *lisp-binder*
                                         object
                                         args-array)))
      (%handle-to-value result-handle))))

(defun set-static (new-value type member-name &rest indexes)
  "Sets the value of the member designated by MEMBER-NAME and
TYPE to NEW-VALUE, and returns NEW-VALUE. TYPE must be a
CLR-OBJECT referring to an object derived from System.Type.  If
the member is a property, the INDEXES are its indexes. If the
member is a field, there must be no optional arguments."
  (%with-clr-handles* ((args-array (%make-args-array indexes
                                                     (1+ (length indexes))))
                       (value-handle (%value-to-handle new-value)))
    ;; Tack the new value on the end of the args array.
    (%handle-to-value (%set-array-element args-array
                                          (length indexes)
                                          value-handle))
    (let ((result-handle
           (%invoke-member type
                           member-name
                           *static-set-prop-or-field-binding-flags*
                           *lisp-binder*
                           nil
                           args-array)))
      (%handle-to-value result-handle)))
  new-value)

(defun set-instance (new-value object member-name &rest indexes)
  "Sets the value of the member designated by MEMBER-NAME and
OBJECT to NEW-VALUE, and returns NEW-VALUE. OBJECT must be a
CLR-OBJECT.  If the member is a property, the INDEXES are its
indexes. If the member is a field, there must be no optional
arguments."
  (%with-clr-handles* ((args-array (%make-args-array indexes
                                                     (1+ (length indexes))))
                       (value-handle (%value-to-handle new-value)))
    ;; Tack the new value on the end of the args array.
    (%handle-to-value (%set-array-element args-array
                                          (length indexes)
                                          value-handle))
    (let ((result-handle
           (%invoke-member nil
                           member-name
                           *instance-set-prop-or-field-binding-flags*
                           *lisp-binder*
                           object
                           args-array)))
      (%handle-to-value result-handle)))
  new-value)

(defun invoke-new (type &rest args)
  "Make a new object of type indicated by TYPE, which must be a
symbol designating a CLR type, a CLR type object, or a
namespace-qualified name string of a CLR type."
  (%with-clr-handle (args-array (%make-args-array args))
    (let ((result-handle
           (%invoke-member type
                           ".ctor"
                           *create-instance-binding-flags*
                           *lisp-binder*
                           nil
                           args-array)))
      (%handle-to-value result-handle))))

(defun invoke-callback-delegate (delegate &rest args)
  "Invoke a callback delegate. Mainly for testing."
  (%with-clr-handle (args-handle (%make-args-array args))
    (%handle-to-value (%invoke-callback delegate args-handle))))

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

;;; This method determines how CLR exceptions are printed.
(defmethod print-object ((x clr-exception) stream)
  (if *print-escape*
      (call-next-method)
      (format stream "~<A Common Language Runtime exception was thrown: ~_~A~:>"
              (invoke-instance (exception-of x) "Message"))))

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


