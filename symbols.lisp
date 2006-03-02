(in-package :cl-clr)

;;;
;;; Functions to create and use symbols that represent CLR tokens.
;;;
;;; These key functions in here are GET-TYPE-SYMBOL and
;;; GET-MEMBER-SYMBOL.  The functions in this file deal only with
;;; namespace-qualified type names.
;;;
;;; Possible enhancements:
;;;
;;; - Automatic caching of candidate member info sets, in a member or
;;;   type symbol property.  See System.Reflection.Binder.BindToMethod
;;;   and BindToField for some ideas on why this would be useful. (For
;;;   properties, perhaps need to use the set/get method names?)
;;;
;;; - Allow strings to be provided where member or type objects are
;;;   now required. Of course, the above-mentioned caching of member
;;;   info won't happen in such cases.
;;;
;;; - Get RDNZL to make static calls more efficient, either by
;;;   allowing a means of giving it an already-available type object,
;;;   or by providing separate calls for static invocations,
;;;   properties, fields.
;;;
;;; - A custom binder could provide a more elegant and correct
;;;   solution to the LispWorks single/double float problem,
;;;   down-converting values only when necessary to find a signature
;;;   match.

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
(defvar *system-type-object* nil)
                     
(defun new (type &rest args)
  (if (symbolp type)
      (setf type (get 'type type)))
  (apply #'rdnzl:new type args))

(defun binding-flag (name)
  (field "System.Reflection.BindingFlags" name))

(defun invoke-member (object member-name &rest args)
  (let ((result
         (if (symbolp object)
             (invoke (get 'type object)
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
      (invoke (get 'type object)
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
  (apply #'invoke-member object member indexes))

(defun get-member-symbol (member-name)
  (let ((symbol (intern (concatenate 'string "." member-name) :clr-symbols)))
    (unless (get 'member symbol)
      (export symbol :clr-symbols)
      (setf (fdefinition symbol)
            #'(lambda (object &rest args)
                (apply #'invoke-member object member-name args)))
      (setf (fdefinition (list 'setf symbol))
            #'(lambda (new-value object &rest args)
                (apply #'set-member new-value object member-name args)))
      (setf (get 'member symbol) t))
    symbol))

(defun get-type-symbol (type-object)
  (let* ((namespace (property type-object "Namespace"))
         (type-name (property type-object "Name"))
         (package-name (concatenate 'string "CLR!" namespace))
         (package   (or (find-package package-name)
                        (make-package package-name))))
    (multiple-value-bind (type-symbol status) (intern type-name package)
      (unless (eq status :external)
        (let (members)
          (do-rdnzl-array (member-info (invoke type-object "GetMembers"))
            ;; TODO: Clean this up, make efficient. Use
            ;; binding flags to limit to members of interest.
            ;; Also, handle nested types if necessary. (?)
            (push (get-member-symbol (property member-info "Name")) members)
          (import members package)
          (export (cons type-symbol members) package)))
        (setf (get 'type type-symbol) type-object))
      type-symbol)))

(defun get-clr-type-object (type)
  (typecase type
    (symbol
     (get 'type type))
    (string (find-type-from-namespace-qualified-name type))))

(defun init-symbols ()
  (flet ((find-and-cache-system-type (name)
           (get-type-symbol (invoke "System.Type" "GetType" name))))
    ;; Here are some primitive type objects that we need in order to
    ;; bootstrap the type lookup code. They are the objects used by
    ;; FIND-TYPE-FROM-NAME and its supporting functions.
    (setf *system-type-object*
          (get 'type (find-and-cache-system-type "System.Type")))
    (find-and-cache-system-type "System.AppDomain"))

  ;; If there are other symbols defined, make sure that they're set to
  ;; something. GET-CLR-SYMBOL will signal an error if a type cannot
  ;; be unambiguously resolved. This is relevant if RDNZL is being
  ;; re-initialized, e.g. in a saved image. As soon as this system is
  ;; initialized, conflicts of referenced types will be detected.
  (do-external-symbols (sym :clr-symbols)
    (when (eq (get 'type sym) t)
      (assert nil)                      ; this needs rewriting
      (setf (get 'type sym)
            (find-type-from-namespace-qualified-name (symbol-name sym)))))

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
  (do-external-symbols (sym :clr-symbols)
    (when (get 'type sym)
      (setf (get 'type sym) t)))
  (setf *system-type-object* nil))

