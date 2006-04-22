;;; $Id$
;;;
;;; Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.
;;;
;;; Functions to bind symbols that represent CLR types and members.
;;;
;;; Policy on the naming of such symbols and their package membership
;;; are defined by the reader, not by this file.

(in-package :cl-clr)

(defvar *namespace-packages* nil
  "List of packages created to represent namespaces.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Argument checking and conversion
;;;

(defun to-clr-type-object (arg)
  "Given a designator for a CLR type, returns the corresponding
CLR System.Type object. The designator can be a symbol created by
CL-CLR (usually via the reader) to designate the type, or a
string containing the full type name. Analogous to C++/CLR's
typeid pseudo-static-field or C#'s typeof() operator."
  (cond ((symbolp arg) (get arg 'clr-type))
        ((stringp arg) (find-type-from-full-name arg))
        (t (error "Expected type designator (a symbol, CLR type object, type name string), but got ~S." arg))))

(defun type-arg-to-type-object (arg)
  "Converts an argument value which is required to be a type
designator to a type object. We accept a symbol, a CLR type
object, or a string as a designator for a CLR type. Contrast with
ARG-TO-TYPE-OBJECT, which accepts only a symbol."
  (cond ((symbolp arg) (to-clr-type-object arg))
        ((clr-object-p arg) arg)
        ((stringp arg) (find-type-from-full-name arg))
        (t (error "Expected type designator (a symbol, CLR type object, type name string), but got ~S." arg))))

(defun arg-to-type-object (arg)
  "Converts an argument value which might be a symbol designating
a CLR type to the corresponding object. If ARG is not such a
symbol, returns NIL."
  (when (and (not (eq arg t)) (symbolp arg))
    (to-clr-type-object arg)))

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

(defun static-member-symbol-p (symbol)
  (get symbol 'clr-static-members))

(defun bind-static-member-symbol (type-symbol member-symbol member-name)
  (let ((members (invoke-instance (to-clr-type-object type-symbol)
                                  "GetMembers"
                                  (binding-flags "Static"
                                                 "Public"
                                                 "FlattenHierarchy"))))
    (when (or (not members) (zerop (invoke-instance members "Length")))
        (error "No static member named ~A of type ~S found."
               member-name
               (invoke-instance (to-clr-type-object type-symbol)
                                "FullName")
               type-symbol))
    ;; TODO: Make use of the cached member info array.
    ;; TODO: Clear cached member info array, re-init on startup.
    (setf (fdefinition member-symbol)
          #'(lambda (&rest args)
              (apply #'invoke-static
                     (to-clr-type-object type-symbol)
                     member-name
                     args)))
    (setf (fdefinition (list 'setf member-symbol))
          #'(lambda (new-value &rest args)
              (apply #'set-static
                     new-value
                     (to-clr-type-object type-symbol)
                     member-name)
              args))
    (setf (get member-symbol 'clr-member) member-name)
    (setf (get member-symbol 'clr-static-members) members))
  member-symbol)

(defun instance-member-symbol-p (symbol)
  (get symbol 'clr-member))

(defun bind-instance-member-symbol (member-symbol member-name)
  (setf (fdefinition member-symbol)
        #'(lambda (object &rest args)
            (apply #'invoke-instance
                   object
                   member-name
                   args)))
  (setf (fdefinition (list 'setf member-symbol))
        #'(lambda (new-value object &rest args)
            (apply #'set-instance
                   new-value
                   object
                   member-name
                   args)))
  (setf (get member-symbol 'clr-member) member-name)
  member-symbol)

(defun type-symbol-p (symbol)
  "Returns T if SYMBOL has been defined as a CLR type
designator."
  (not (null (get symbol 'clr-type))))

(defun bind-type-symbol (type-symbol type-object)
  "Given a symbol and a CLR type object, cache the object on
symbol as a property, thus making the symbol a valid CLR type
designator. Returns the TYPE-SYMBOL."
  (setf (get type-symbol 'clr-type) type-object)
  type-symbol)
 
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

