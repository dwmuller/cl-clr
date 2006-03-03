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
                     
(defun new (type &rest args)
  (if (symbolp type)
      (setf type (get type 'clr-type)))
  (apply #'rdnzl:new type args))

(defun binding-flag (name)
  (field "System.Reflection.BindingFlags" name))

(defun invoke-member (object member-name &rest args)
  (let ((result
         (if (symbolp object)
             (invoke (get object 'clr-type)
                     "InvokeMember"
                     member-name
                     *static-member-binding-flags*
                     *default-binding-object*
                     *null-object*
                     (list-to-rdnzl-array args))
             (invoke (invoke object "GetType")
                     "InvokeMember"
                     member-name
                     *instance-member-binding-flags*
                     *default-binding-object*
                     object
                     (list-to-rdnzl-array args)))))
    (if result
        (unbox (cast result (invoke result "GetType")))
        result)))
  
(defun set-member (new-value object member-name &rest indexes)
  ;; This could be done much faster using some of RDNZL's internals.
  (if (symbolp object)
      ;; Note: Unlike in RDNZL, we have the type object
      ;; already.
      (invoke (get object 'clr-type)
              "InvokeMember"
              member-name
              *static-set-prop-or-field-binding-flags*
              *default-binding-object*
              *null-object*
              ;; Sadly, new-value has to come after the indexes.
              (list-to-rdnzl-array (append indexes (list new-value))))
      (invoke (invoke object "GetType")
              "InvokeMember"
              member-name
              *instance-set-prop-or-field-binding-flags*
              *default-binding-object*
              object
              ;; Sadly, new-value has to come after the indexes.
              (list-to-rdnzl-array (append indexes (list new-value)))))
  (apply #'invoke-member object member-name indexes))

(defun get-member-symbol (member-name)
  "Given the name of a CLR member, return a symbol to represent
it. The symbol is interned in the CLR-SYMBOLS package, and has no
inherent relationship with any type."
  (let ((symbol (intern (concatenate 'string "." member-name) :clr-symbols)))
    (unless (get symbol 'clr-member)
      (export symbol :clr-symbols)
      (setf (fdefinition symbol)
            #'(lambda (object &rest args)
                (apply #'invoke-member object member-name args)))
      (setf (fdefinition (list 'setf symbol))
            #'(lambda (new-value object &rest args)
                (apply #'set-member new-value object member-name args)))
      (setf (get symbol 'clr-member) t))
    symbol))

(defun get-namespace-package (namespace)
  (let* ((pkg-name (concatenate 'string "CLR!" namespace))
         (pkg (find-package pkg-name)))
    (when (not pkg)
      (setf pkg (make-package pkg-name))
      ;(format t "~&New namespace package ~S." (package-name pkg))
      (push pkg *namespace-packages*))
    pkg))
    
    
(defun get-type-symbol (type)
  "TYPE must be a namespace-qualified type name string or a CLR
type object. Returns a symbol to represent the type. The symbol
is interned in a namespace package, and symbols for its members
are imported into that package from CLR-SYMBOLS."
  (let* ((type-object (cond ((stringp type)
                             (find-type-from-namespace-qualified-name type))
                            ((container-p type) type)
                            (t (error "Expected TYPE to be a CLR type object or qualified type name string."))))
         (namespace (property type-object "Namespace"))
         (type-name (property type-object "Name"))
         (package  (get-namespace-package namespace)))
    (multiple-value-bind (type-symbol status) (intern type-name package)
      (unless (eq status :external)
        (let (members)
          (do-rdnzl-array (member-info (invoke type-object "GetMembers"))
            (push (get-member-symbol (property member-info "Name")) members)
          (import members package)
          (export (cons type-symbol members) package)))
        (setf (get type-symbol 'clr-type) type-object))
      type-symbol)))

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
                            (error "~S is not a CLR type symbol." type)))
        ((container-p type) (invoke type "GetType"))
        (t (error "Expected string, symbol, or CLR object for TYPE."))))

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

