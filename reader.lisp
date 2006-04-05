;;; $Id$
;;;
;;; Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.
;;;
;;; This reader provides an alternative to RDNZL's reader syntax. It
;;; uses a symbol-based approach, rather than form-based. More
;;; importantly, it resolves unqualified type names at read time, once
;;; and for all. This has two advantages: Once a type has been
;;; associated with a symbol, that type name won't have to be looked
;;; up at run time anymore. Secondly, changes to the list of used
;;; namespaces won't change the run-time interpretation of a type
;;; name.
;;;
;;; CLR names are prefixed with a question mark.
;;;
;;; A plus sign immediately after the question mark introduces a type
;;; name. Otherwise, a member name is assumed. Each is replaced by a
;;; symbol in package CLR-SYMBOLS. In the case of members, that symbol
;;; is also imported into the current package.
;;;
;;; The form for calling a static method or referencing a static field
;;; or property is:
;;;
;;;   (?.method '?type args*)
;;;
;;; In other words, static method invocations are exactly like
;;; instance method invocations, except that the 'object' is a symbol
;;; designating a CLR type.
;;;
;;; Reader syntax expansions:
;;;
;;; What             Syntax                   Expansion
;;; -----------------------------------------------------------------------
;;; type:            ?typename               symbol
;;; member:          ?.membername              symbol
;;;

(in-package :cl-clr)

(defclass reader-context ()
  ((previous-readtable :accessor precedent-of  :initform *readtable*)
   (referenced-types   :accessor types-of      :initform nil)
   (referenced-members :accessor members-of    :initform nil)
   (used-namespaces    :accessor namespaces-of :initform nil)))

(defvar *contexts* nil
  "A stack which holds contexts for the CL-CLR reader. The
topmost context is the current one.")

(defun current-context ()
  (car *contexts*))

(defun push-context ()
  (push (make-instance 'reader-context) *contexts*))

(defun pop-context ()
  (cond
    (*contexts* (setf *readtable* (precedent-of (car *contexts*)))
                (pop *contexts*))
    (t (setf *readtable* (copy-readtable nil))))
  (values))

(defconstant +whitespace-char-list+
             '(#\Space #\Tab #\Linefeed #\Newline #\Return #\Page)
  "A list of all characters which are considered to be whitespace.")

(defun whitespacep (chr)
  "Tests whether a character is whitespace."
  (member chr +whitespace-char-list+ :test #'char=))

(defun lookup-type-symbol (type-name)
  (clr-type-object-to-symbol
   (find-type-from-name type-name
                        (namespaces-of (current-context)))))

(defun read-clr-name (stream)
  (loop
     with escaped  = nil
     with name = (make-array '(0) :element-type 'base-char
                             :fill-pointer 0 :adjustable t)
     for char = (peek-char nil stream nil nil t)
     when (and escaped (not char))
     do (error "EOF encountered after escape (\\) in CLR symbol.")
     until (and (not escaped)
                (or (not char)          ;eof
                    (whitespacep char)
                    (eql #\) char)))         ;end of a list
     do
       ;; Note that we keep escape characters in the result.  Some
       ;; characters have special meaning in CLR names, and users need
       ;; a way to escape them. Unneeded escape characters are ignored
       ;; by the framework when we look up types.
       (cond
         (escaped         (setf escaped nil))
         ((eql char #\\)  (setf escaped t)))
       (vector-push-extend char name)
       (read-char stream t nil t)       ;consume the peeked char
     finally (return name)))

(defun read-clr-member (stream &optional char)
  (declare (ignorable char))
  (let ((symbol (get-member-symbol (read-clr-name stream))))
    (pushnew symbol (members-of (current-context)))
    symbol))

(defun read-clr-type (stream &optional char)
  (declare (ignorable char))
  (let ((symbol (lookup-type-symbol (read-clr-name stream))))
    (pushnew symbol (types-of (current-context)))
    symbol))

(defun read-clr-token (stream &optional char)
  (declare (ignorable char))
  (let ((next-char (peek-char nil stream nil nil t)))
    (if (eql #\. next-char)
        (read-clr-member stream (read-char stream t nil t))
        (read-clr-type stream char))))

;----------------------------------------------------------------------------

(defmacro enable-clr-syntax (&rest namespaces)
  "Enable reader syntax and push a new context for it,
initializing the list of namespace to NAMESPACES. (See
USE-NAMESPACES.) If the reader syntax was previously enabled, the
previous list is saved, and is restored after a call to
BIND-CLR-SYMBOLS. A call to USE-NAMESPACES should usually appear
at the top of a file. The file should end with a call to
BIND-CLR-SYMBOLS."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *readtable* (copy-readtable))
     (set-macro-character #\? #'read-clr-token t)
     (push-context)
     (use-namespaces ,@namespaces)
     (values)))

(defmacro use-namespace (namespace)
  "Adds NAMESPACE, which is a namespace name string, to the list
of namespaces to be searched by the reader to resolve unqualified
type names."
  (let ((ns (gensym)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,ns ,namespace))
         (check-type ,ns string)
         ;; Make sure that the namespace package exists. This is
         ;; especially important when loading a compiled file that uses
         ;; USE-NAMESPACES.
         (get-namespace-package ,ns)
         (push ,ns (namespaces-of (current-context)))
         (values)))))

(defmacro use-namespaces (&rest namespaces)
  "Adds NAMESPACES, which are namespace name strings, to the list
of namespaces to be searched by the reader to resolve unqualified
type names."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (loop
        for namespace in ',namespaces
        do
          (use-namespace namespace))
     (values)))

(defmacro bind-clr-symbols (&optional dump)
  "Disables the reader syntax, and discards the list of
namespaces to be searched by the reader. Also arranges, at load
time, to search for any referenced types in currently loaded
assemblies and bind them to Lisp symbols that were produced by
the reader. This should only be called after USE-NAMESPACE, and
usually as the last top-level form in a file."
  `(progn
     (eval-when (:load-toplevel :execute)
       (map nil #'bind-type-symbol ',(types-of (current-context)))
       (map nil #'bind-member-symbol ',(members-of (current-context)))
     (eval-when (:compile-toplevel :execute)
       ,@(when dump
              `((format t "~&The following CLR types were referenced:~%~{  ~A~%~}"
                        (map 'list #'symbol-to-clr-type-name
                           ',(types-of (current-context))))
                (format t "~&The following CLR members were referenced:~%~{  ~A~%~}"
                        (map 'list #'symbol-name
                             ',(members-of (current-context))))))
       (pop-context)))
     (values)))