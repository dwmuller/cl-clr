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

(defun invoke-static (type member-name &rest args)
  "Invokes or retrieves the value of the static member designated
by MEMBER-NAME and TYPE. TYPE must be a CLR-OBJECT referring to
an object derived from System.Type.  If the member is a property,
the ARGS are its indexes. If the member is a method, ARGS are the
arguments. If the member is a field, there must be no optional
arguments."
  (%with-clr-handle (args-array (%make-args-array args (length args)))
    (generic-invoke-member type
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
    (generic-invoke-member nil
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
                       (new-handle (%box new-value)))
    ;; Tack the new value on the end of the args array.
    (%set-array-element args-array (length indexes) new-handle)
    (generic-invoke-member type
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
                       (new-handle (%box new-value)))
    ;; Tack the new value on the end of the args array.
    (%set-array-element args-array (length indexes) new-handle)
    (generic-invoke-member nil
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
    (generic-invoke-member type
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

;; Keep test here for now. Eventually we'll test via the public
;; functions.
(defun test ()
  (let ((root (%make-clr-object (%get-default-app-domain))))
    (print
     (invoke-instance (invoke-instance (invoke-instance root
                                                        "GetType")
                                       "GetType")
                      "FullName"))))
                    

