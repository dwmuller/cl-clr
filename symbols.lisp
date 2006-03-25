;;; $Header:$
;;;
;;; Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.
;;;
;;; Functions to create and use symbols that represent CLR tokens.
;;;
;;; These key functions in here are GET-TYPE-SYMBOL and
;;; GET-MEMBER-SYMBOL.  The functions in this file deal only with
;;; namespace-qualified type names.
;;;
(in-package :cl-clr)

(defvar *namespace-packages* nil
  "List of packages created to represent namespaces.")

(defvar *null-object* nil
  "A null object for use in various generated functions.")

(defvar *empty-object-array* nil
  "A null object array for use in various generated functions.")

(defvar *default-binding-object* nil
  "A default binding object for use in invocation calls.")

(defvar *static-member-binding-flags* nil)
(defvar *static-set-prop-or-field-binding-flags* nil)
(defvar *instance-member-binding-flags* nil)
(defvar *instance-set-prop-or-field-binding-flags* nil)

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
                     
(define-rdnzl-call generic-invoke-member
    (:dotnet-name "InvokeMember")
  ((type "System.Type")
   (name "System.String")
   (invoke-attr "System.Reflection.BindingFlags")
   (binder "System.Reflection.Binder")
   (target "System.Object")
   (args   "System.Object[]")))

(define-rdnzl-call get-type
    ()
  ((object "System.Object")))

(defun new (type &rest args)
  "Make a new object of type indicated by TYPE, which must be a
symbol designating a CLR type, a CLR type object, or a
namespace-qualified name string of a CLR type."
  (apply #'rdnzl:new
         (cond 
           ((symbolp type) (get-type-object type))
           ((container-p type) type)
           ((stringp type) (find-type-from-namespace-qualified-name type))
           (t (error "~S is not a symbol, CLR object, or string designating a CLR type."
                     type)))
         args))

(defun make-null-object (type)
  (rdnzl:make-null-object
   (property
    (cond
      ((symbolp type) (get-type-object type))
      ((container-p type) type)
      ((stringp type) (find-type-from-namespace-qualified-name type))
      (t (error "~S is not a symbol, CLR object, or string designating a CLR type."
                type)))
    "FullName")))
                            

(defun binding-flag (name)
  (field "System.Reflection.BindingFlags" name))

(defun force-type (obj)
  "A hack to experiment with always using the run-time type,
  rather than the apparent type, for containers."
  (if (container-p obj)
      (cast obj (get-type obj))
      obj))

(defun invoke-member (object member-name &rest args)
  (let ((result
         (rdnzl-handler-case
          (if (symbolp object)
              (generic-invoke-member (get object 'clr-type)
                                     member-name
                                     *static-member-binding-flags*
                                     *default-binding-object*
                                     *null-object*
                                     (list-to-rdnzl-array args))
              (generic-invoke-member (get-type object)
                                     member-name
                                     *instance-member-binding-flags*
                                     *default-binding-object*
                                     object
                                     (list-to-rdnzl-array args)))
          ("System.Reflection.TargetInvocationException"
           (err)
           ;; Unwrap the inner exception; the caller isn't interested
           ;; in the fact that the call occurred via reflection.
           (signal 'rdnzl-error
                   :exception
                   (property err "InnerException"))))))
        (if (container-p result)
            (unbox (force-type result))
            result)))
  
(defun set-member (new-value object member-name &rest indexes)
  (rdnzl-handler-case
   (if (symbolp object)
       ;; Note: Unlike in RDNZL, we have the type object
       ;; already.
       (generic-invoke-member (get object 'clr-type)
                              member-name
                              *static-set-prop-or-field-binding-flags*
                              *default-binding-object*
                              *null-object*
                              ;; Sadly, new-value has to come after the indexes.
                              (list-to-rdnzl-array (append indexes
                                                           (list new-value))))
       (generic-invoke-member (get-type object)
                              member-name
                              *instance-set-prop-or-field-binding-flags*
                              *default-binding-object*
                              object
                              ;; Sadly, new-value has to come after the indexes.
                              (list-to-rdnzl-array (append indexes
                                                           (list new-value)))))
   ("System.Reflection.TargetInvocationException"
    (err)
    ;; Unwrap the inner exception; the caller isn't interested
    ;; in the fact that the call occurred via reflection.
    (signal 'rdnzl-error
            :exception
            (property err "InnerException"))))
  new-value)

(defun get-namespace-package (namespace)
  "Given a CLR namespace name, return the package that represents
  it in Lisp. Note "
  (let* ((pkg-name (concatenate 'string "CLR!" namespace))
         (pkg (find-package pkg-name)))
    (when (not pkg)
      (setf pkg (make-package pkg-name))
      ;(format t "~&New namespace package ~S." (package-name pkg))
      (push pkg *namespace-packages*))
    pkg))

(defun bind-member-symbol (member-symbol &optional member-name)
  "Makes a symbol usable as a CLR member designator. Returns the
symbol. Unless member-name is supplied, the symbol's name must be
a CLR member name, with a preceding period."
  (unless member-name
    (setf member-name (subseq (symbol-name member-symbol) 1)))
  (setf (fdefinition member-symbol)
        #'(lambda (object &rest args)
            (apply #'invoke-member object member-name args)))
  (setf (fdefinition (list 'setf member-symbol))
        #'(lambda (new-value object &rest args)
            (apply #'set-member new-value object member-name args)))
  (setf (get member-symbol 'clr-member) t)
  member-symbol)
  
(defun get-member-symbol (member-name)
  "Given the name of a CLR member, return a symbol to represent
it. The symbol is interned in the CLR-SYMBOLS package."
  (let ((symbol (intern (concatenate 'string "." member-name) :clr-symbols)))
    (export symbol :clr-symbols)
    (unless (get symbol 'clr-member)
      (bind-member-symbol symbol member-name))
    symbol))

(defun get-type-name-from-symbol (sym)
  (let* ((pkg             (symbol-package sym))
         (pkg-name        (and pkg (package-name pkg)))
         (namespace-name  (and (eql 4 (mismatch pkg-name "CLR!"))
                               (subseq pkg-name 4))))
    (unless namespace-name
      (error "Symbol ~S does not designate a CLR type." sym))
     (concatenate 'string namespace-name "." (symbol-name sym))))
     
(defun bind-type-symbol (type-symbol &optional type-object)
  "Given a symbol whose home package represents a namespace, and
whose name is a CLR type name in that namespace, bind the symbol
as a designator for that CLR type. Returns the symbol."
  (unless type-object
    (setf type-object
          (get-type-object (get-type-name-from-symbol type-symbol))))
  (let (members)
    (do-rdnzl-array (member-info (invoke type-object "GetMembers"))
      (pushnew (get-member-symbol (property member-info "Name")) members))
    (setf (get type-symbol 'clr-members) members))
  (setf (get type-symbol 'clr-type) (force-type type-object))
  type-symbol)
 
(defun get-type-symbol (type)
  "TYPE must be a namespace-qualified type name string or a CLR
type object. Returns a symbol to represent the type. The symbol
is interned in a namespace package, and symbols for its members
are imported into that package from CLR-SYMBOLS."
  (let ((type-object (cond ((stringp type)
                            (find-type-from-namespace-qualified-name type))
                           ((container-p type) type)
                           (t (error "Expected TYPE to be a CLR type object or qualified type name string.")))))
    (multiple-value-bind (namespace type-name)
        (split-type-name (property type-object "FullName"))
      (let ((package     (get-namespace-package namespace)))
        (multiple-value-bind (type-symbol status) (intern type-name package)
          (unless (eq status :external)
            (bind-type-symbol type-symbol type-object)
            (export type-symbol package)
            (import (get type-symbol 'clr-members) package)
            (export (get type-symbol 'clr-members) package))
          type-symbol)))))

(defun get-type-object (type)
  "Given the namespace-qualified name string of a CLR type,
returns corresponding System.Type object if it can be found in
the loaded assemblies of the current application domain. Given a
symbol representing a CLR type, returns a corresponding
System.Type object. Given a CLR object, returns a System.Type
object representing that object's type. Signals an error in all
other cases."
  (cond ((stringp type) (or (find-type-from-namespace-qualified-name type)
                            (error "~S does not name an accessible type.")))
        ((symbolp type) (or (get type 'clr-type)
                            (setf (get type 'clr-type)
                                  (find-type-from-namespace-qualified-name
                                   (get-type-name-from-symbol type)))))
        ((container-p type) (force-type (get-type type)))
        (t (error "Expected string, symbol, or CLR object for TYPE."))))

(defun bind-namespace (namespace)
  "Binds all type and member symbols of a namespace which can be
found in the currently loaded assemblies of the current
application domain.  Returns the namespace package."
  (let ((package (get-namespace-package namespace)))
    (do-rdnzl-array (assembly (invoke (property "System.AppDomain"
                                                "CurrentDomain")
                                      "GetAssemblies"))1
      (do-rdnzl-array (type-object (invoke assembly "GetTypes"))
        (when (zerop (search namespace (property type-object "FullName")))
          (get-type-symbol type-object) package)))
    package))

(defmacro def-namespaces (&rest namespaces)
  "Used to declare the namespaces that will be referenced by a program.
Ensures that a package exists to represent each namespace. If you
use a separate file to define packages for you program, it is
recommended that you include a def-namespaces form in this same
file."
  (eval-when (:compile-toplel :load-toplevel :execute)
    `(map nil #'get-namespace-package ',namespaces)))


(defun import-type (type &optional (package *package*))
  "TYPE is a symbol or string designating a CLR
type. IMPORT-TYPES imports the symbol designating the CLR type to
PACKAGE, which defaults to *PACKAGE*. It also imports symbols to
represent all public members of the CLR type. Returns the symbol
designating the CLR type."
  (setf package (find-package *package*))
  (when (stringp type)
    (setf type (get-type-symbol type)))
  (import type package)
  (import (get type 'clr-members) package)
  type)

(defun init-symbols ()
  (flet ((find-and-cache-system-type (name)
           (get-type-symbol (invoke "System.Type" "GetType" name))))
    ;; Here are some primitive type objects that we need in order to
    ;; bootstrap the type lookup code. They are the objects used by
    ;; GET-TYPE-SYMBOL and its supporting functions to turn names into
    ;; type objects..
    (find-and-cache-system-type "System.Type")
    (find-and-cache-system-type "System.AppDomain"))

  ;; If there are other symbols defined, make sure that they're set to
  ;; something. GET-CLR-SYMBOL will signal an error if a type cannot
  ;; be unambiguously resolved. This is relevant if RDNZL is being
  ;; re-initialized, e.g. in a saved image. As soon as this system is
  ;; initialized, conflicts of referenced types will be detected.
  (dolist (pkg *namespace-packages*)
    (do-external-symbols (sym pkg)
      (when (stringp (get sym 'clr-type))
        ;(format t "~&Initializing type symbol ~S." sym)
        (setf (get sym 'clr-type)
              (find-type-from-namespace-qualified-name (get sym 'clr-type))))))

  ;; Initialize some local stuff referenced by closures we create.
  (setf *null-object* (make-null-object "System.Object"))
  (setf *default-binding-object*
        (property "System.Type" "DefaultBinder"))
  (setf *empty-object-array* (list-to-rdnzl-array nil "System.Object"))
  (setf *static-member-binding-flags*
        (or-enums (binding-flag "Static")
                  (binding-flag "GetProperty")
                  (binding-flag "GetField")
                  (binding-flag "InvokeMethod")
                  (binding-flag "Public")))
  (setf *static-set-prop-or-field-binding-flags*
        (or-enums (binding-flag "Static")
                  (binding-flag "SetProperty")
                  (binding-flag "SetField")
                  (binding-flag "Public")))
  (setf *instance-member-binding-flags*
        (or-enums (binding-flag "Instance")
                  (binding-flag "GetProperty")
                  (binding-flag "GetField")
                  (binding-flag "InvokeMethod")
                  (binding-flag "Public")))
  (setf *instance-set-prop-or-field-binding-flags*
        (or-enums (binding-flag "Instance")
                  (binding-flag "SetProperty")
                  (binding-flag "SetField")
                  (binding-flag "Public"))))

(defun release-symbols ()
  "Releases all CLR objects cached by the symbols subsystem of
CL-CLR. Must be called when shutting down, e.g. to save an
image."
  (setf *instance-set-prop-or-field-binding-flags* nil)
  (setf *instance-member-binding-flags* nil)
  (setf *static-set-prop-or-field-binding-flags* nil)
  (setf *static-member-binding-flags* nil)
  (setf *empty-object-array* nil)
  (setf *default-binding-object* nil)
  (setf *null-object* nil)
  (dolist (pkg *namespace-packages*)
    (do-external-symbols (sym pkg)
      (let ((type-object (get sym 'clr-type)))
        (when (container-p type-object)
          ;(format t "~&Shutting down type symbol ~S." sym)
          (setf (get sym 'clr-type)
                (elide-assembly (property type-object "FullName"))))))))

