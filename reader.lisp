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

(defvar *previous-readtables* nil
  "A stack which holds previous readtables that were pushed down
by ENABLE-CLR-SYNTAX.")

(defconstant +whitespace-char-list+
             '(#\Space #\Tab #\Linefeed #\Newline #\Return #\Page)
  "A list of all characters which are considered to be whitespace.")

(defun whitespacep (chr)
  "Tests whether a character is whitespace."
  (member chr +whitespace-char-list+ :test #'char=))

(defun lookup-type-symbol (type-name)
  (get-type-symbol
   (find-type-from-name type-name
                        (namespaces-used-by-package *package*))))

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
  (let ((member-symbol (get-member-symbol (read-clr-name stream))))
    (import member-symbol *package*)
    member-symbol))

(defun read-clr-type (stream &optional char)
  (declare (ignorable char))
  (let ((type-symbol (lookup-type-symbol (read-clr-name stream))))
    (import type-symbol *package*)
    type-symbol))

(defun read-clr-token (stream &optional char)
  (declare (ignorable char))
  (let ((next-char (peek-char nil stream nil nil t)))
    (if (eql #\. next-char)
        (read-clr-member stream (read-char stream t nil t))
        (read-clr-type stream char))))

(defun %enable-clr-syntax (&optional (macro-char #\?))
  "Internal function used to enable reader syntax and store
current readtable on stack. MACRO-CHAR is the character used to
prefix CLR names, and defaults to the question mark."
  (setf *readtable* (copy-readtable))
  (push *readtable*
        *previous-readtables*)
  (set-macro-character macro-char #'read-clr-token t)
  (values))

(defun %disable-clr-syntax ()
  "Internal function used to restore previous readtable. "
  (if *previous-readtables*
      (setf *readtable* (pop *previous-readtables*))
      (setf *readtable* (copy-readtable nil)))
  (values))

(defmacro enable-clr-syntax ()
  "Enables CLR reader syntax."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-clr-syntax)))

(defmacro disable-clr-syntax ()
  "Restores the readtable which was active before the last call to
ENABLE-CLR-SYNTAX. If there was no such call, the standard readtable
is used."
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (%disable-clr-syntax)))
