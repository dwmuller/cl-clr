;;; $Id$
;;;
;;; Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.
;;;
;;; Functions to create and use symbols that represent CLR types and
;;; members.
;;;

(in-package :cl-clr)

(defvar *namespace-packages* nil
  "List of packages created to represent namespaces.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic conversions to/from type symbols.
;;;
(defun symbol-to-clr-type-name (sym)
  "Given a symbol designating a CLR type, returns the
namespace-qualifier name string of the type."
  (let* ((pkg             (symbol-package sym))
         (pkg-name        (and pkg (package-name pkg)))
         (namespace-name  (and (eql 4 (mismatch pkg-name "CLR!"))
                               (subseq pkg-name 4))))
    (unless namespace-name
      (error "Symbol ~S does not designate a CLR type." sym))
     (concatenate 'string namespace-name "." (symbol-name sym))))
     
(defun symbol-to-clr-type-object (arg)
  "ARG must be a symbol designating a CLR type object. Returns
the type object."
  (unless (symbolp arg)
    (error "Expected string, symbol, or CLR object, but got ~S." arg))
  (or (get arg 'clr-type)
      (setf (get arg 'clr-type)
            (find-type-from-full-name (symbol-to-clr-type-name arg)))))

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


(defun clr-type-object-to-symbol (type-object)
  (multiple-value-bind (namespace type-name)
      (split-type-name (invoke-member type-object "FullName"))
    (let ((package     (get-namespace-package namespace)))
      (multiple-value-bind (type-symbol status)
          (intern type-name package)
        (unless (eq status :external)
          (bind-type-symbol type-symbol type-object)
          (export type-symbol package)
          (import (get type-symbol 'clr-members) package)
          (export (get type-symbol 'clr-members) package))
        type-symbol))))

(defun clr-type-name-to-symbol (type-name &optional namespaces)
  "Returns the symbol that denotes the CLR type described by the
string TYPE-NAME. NAMESPACES is an optional list of namespace
name strings which will be searched if TYPE-NAME is not
namespace-qualified. TYPE-NAME may include an assembly
designator."
  (check-type type-name string)
  ;; This is not fast, since it may do a search for the type through
  ;; all assemblies and namespaces.
  (let* ((type-object (find-type-from-name type-name namespaces)))
    (clr-type-object-to-symbol type-object)))


  #|
  ;; TODO: If the assembly is given, and we find a symbol for the name
  ;; but it's for a type from a different assembly, we will return the
  ;; symbol even though it specifies the wrong type. In a case like
  ;; this, we actually have a type name clash.
  (multiple-value-bind (namespace simple-type-name)
      (split-type-name type-name)
    (when namespace
      (setf namespaces (list namespace)))
    (let ((type-symbol
           (loop
              for namespace in namespaces
              thereis (find-symbol simple-type-name
                                   (get-namespace-package namespace)))))
      (unless type-symbol
        (setf  type-symbol (clr-type-object-to-symbol
                            (find-type-from-name type-name namespaces))))
      type-symbol))) |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Argument checking and conversion
;;;
(defun type-arg-to-type-object (arg)
  "Converts an argument value which is required to be a type
designator to a type object. We accept a symbol, a CLR type
object, or a string as a designator for a CLR type. Contrast with
ARG-TO-TYPE-OBJECT, which accepts only a symbol."
  (cond ((symbolp arg) (symbol-to-clr-type-object arg))
        ((clr-object-p arg) arg)
        ((stringp arg) (find-type-from-full-name arg))
        (t (error "Expected type designator (a symbol, CLR type object, type name string), but got ~S." arg))))

(defun arg-to-type-object (arg)
  "Converts an argument value which might be a symbol designating
a CLR type to the corresponding object. If ARG is not such a
symbol, returns NIL."
  (when (and (not (eq arg t)) (symbolp arg))
    (symbol-to-clr-type-object arg)))

(defun clr-type-of (obj)
  "Given a CLR object, returns the symbol denoting the object's
CLR type. Analogous to Common Lisp's TYPE-OF."
  (unless (clr-object-p obj)
    (error "Expected a CLR object, got ~S." obj))
  (clr-type-object-to-symbol (invoke-member obj "GetType")))

(defun clr-type-id-of (arg)
  "Given a designator for a CLR type, returns the corresponding
CLR System.Type object. The designator can be a symbol created by
CL-CLR (usually via the reader) to designate the type, or a
string containing the full type name. Analogous to C++/CLR's
typeid pseudo-static-field."
  (cond ((symbolp arg) (symbol-to-clr-type-object arg))
        ((stringp arg) (find-type-from-full-name arg))
        (t (error "Expected type designator (a symbol, CLR type object, type name string), but got ~S." arg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Invocation functions that use symbols to denote types.
;;;
(defun new (type &rest args)
  "Make a new object of type indicated by TYPE, which must be a
symbol designating a CLR type, a CLR type object, or a
namespace-qualified name string of a CLR type. If TYPE designates
a delegate type, then the only additional argument should be a
Lisp function object."
  (if (and (eql (length args) 1) (typep (first args) 'function))
      (make-delegate (first args) (type-arg-to-type-object type))
      (apply #'invoke-new (type-arg-to-type-object type) args)))

(defun invoke-member (object member-name &rest args)
  (let ((type (arg-to-type-object object)))
    (if type
        (apply #'invoke-static type member-name args)
        (apply #'invoke-instance object member-name args))))
  
(defun set-member (new-value object member-name &rest indexes)
  (let ((type (arg-to-type-object object)))
    (if type
      (apply #'set-static new-value type member-name indexes)
      (apply #'set-instance new-value object member-name indexes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Member symbols
;;;
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
  (setf (get member-symbol 'clr-member) member-name)
  member-symbol)
  
(defun get-member-symbol (member-name)
  "Given the name of a CLR member, return a symbol to represent
it. The symbol is interned in the CLR-SYMBOLS package."
  (let ((symbol (intern (concatenate 'string "." member-name) :clr-symbols)))
    (export symbol :clr-symbols)
    (unless (get symbol 'clr-member)
      (bind-member-symbol symbol member-name))
    symbol))

(defun bind-type-symbol (type-symbol &optional type-object)
  "Given a symbol whose home package represents a namespace, and
whose name is a CLR type name in that namespace, bind the symbol
as a designator for that CLR type. Returns the symbol."
  (unless type-object
    (setf type-object
          (find-type-from-full-name (symbol-to-clr-type-name type-symbol))))
  (let (members)
    (do-clr-array (member-info (invoke-member type-object "GetMembers"))
      (pushnew (get-member-symbol (invoke-member member-info "Name")) members))
    (setf (get type-symbol 'clr-members) members))
  (setf (get type-symbol 'clr-type) type-object)
  type-symbol)
 
(defun bind-namespace (namespace)
  "Binds all type and member symbols of a namespace which can be
found in the currently loaded assemblies of the current
application domain.  Returns the namespace package."
  (check-type namespace string)
  (let ((package (get-namespace-package namespace)))
    (do-clr-array (assembly (invoke-member (invoke-member "System.AppDomain"
                                                          "CurrentDomain")
                                           "GetAssemblies"))
      (do-clr-array (type-object (invoke-member assembly "GetTypes"))
        (when (zerop (search namespace (invoke-member type-object "FullName")))
          (clr-type-object-to-symbol type-object) package)))
    package))

(defun import-type (type &optional (package *package*))
  "TYPE is a symbol or string designating a CLR
type. IMPORT-TYPES imports the symbol designating the CLR type to
the package designated by PACKAGE, which defaults to
*PACKAGE*. It also imports symbols to represent all public
members of the CLR type. Returns the symbol designating the CLR
type."
  (let ((type-symbol (clr-type-object-to-symbol
                      (type-arg-to-type-object type))))
    (import type-symbol package)
    (import (get type-symbol 'clr-members) package)
    type-symbol))

(defun init-symbols (&optional dump)
  ;; If there are type symbols defined, make sure that they're set to
  ;; something. This is relevant when the system is being
  ;; re-initialized e.g. when loading and initializing a saved
  ;; image.
  (dolist (pkg *namespace-packages*)
    (do-external-symbols (sym pkg)
      (let ((type-name (get sym 'clr-type)))
        (when (stringp type-name)
          (when dump
            (format t "~&Initializing type symbol ~S to ~S." sym type-name))
          (setf (get sym 'clr-type)
                (find-type-from-full-name type-name)))))))


(defun shutdown-symbols ()
  "Releases all CLR objects cached by the symbols subsystem of
CL-CLR. Must be called when shutting down, e.g. to save an
image."
  (dolist (pkg *namespace-packages*)
    (do-external-symbols (sym pkg)
      (let ((type-object (get sym 'clr-type)))
        (when (clr-object-p type-object)
          ;(format t "~&Shutting down type symbol ~S." sym)
          (setf (get sym 'clr-type)
                (elide-assembly (invoke-instance type-object "FullName"))))))))

