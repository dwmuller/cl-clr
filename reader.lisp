;;; $Id$
;;;
;;; Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.
;;;

(in-package :cl-clr)

(defclass reader-context ()
  ((previous-readtable :accessor precedent-of
                       :initform *readtable*)
   (type-symbols       :accessor types-of
                       :initform nil)
   (static-symbols     :accessor static-members-of
                       :initform nil)
   (instance-symbols   :accessor instance-members-of
                       :initform nil)
   (used-namespaces    :accessor namespaces-of
                       :initform nil)
   (errors             :accessor errors-of
                       :initform nil)))

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

(defun read-clr-token (stream)
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
                    (eql #\: char)
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
          (export type-symbol package))
        type-symbol))))

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

(defun clr-type-of (obj)
  "Given a CLR object, returns the symbol denoting the object's
CLR type. Analogous to Common Lisp's TYPE-OF."
  (unless (clr-object-p obj)
    (error "Expected a CLR object, got ~S." obj))
  (clr-type-object-to-symbol (invoke-member obj "GetType")))

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
    type-symbol))

(defun split-identifier (name)
  (let (last-dot)
    (loop
       with start = 0
       for pos = (unescaped-char-position #\. name :start start)
       until (not pos)
       do
         (setf last-dot pos)
         (setf start (1+ pos)))
    (if last-dot
        (values (subseq name 0 last-dot)
                (subseq name (1+ last-dot)))
        (values nil name))))
  
(defun make-type-symbol (type-name namespace-name &optional type-object)
  (unless type-object
    (let ((full-type-name (concatenate 'string namespace-name "." type-name)))
      (setf type-object (find-type-from-full-name full-type-name))))
    (let ((symbol (intern type-name
                          (get-namespace-package
                           (invoke-instance type-object "Namespace")))))
      (unless (type-symbol-p symbol)
        (bind-type-symbol symbol type-object))
      (pushnew (list symbol (invoke-instance type-object
                                             "AssemblyQualifiedName"))
               (types-of (current-context))
               :key #'first)
      symbol))
  
(defun lookup-type-symbol (type-name)
  (let* ((type-object (find-type-from-name type-name
                                           (namespaces-of (current-context)))))
    (make-type-symbol type-name
                      (invoke-instance type-object "Namespace")
                      type-object)))
        
(defun make-static-member-symbol (type-name member-name namespace)
  (let* ((type-symbol
          (if namespace
              (make-type-symbol type-name namespace)
              (lookup-type-symbol type-name)))
         (member-symbol (intern
                         (concatenate 'string
                                      type-name
                                      "."
                                      member-name)
                         (symbol-package type-symbol))))
    (unless (static-member-symbol-p member-symbol)
      (bind-static-member-symbol type-symbol
                                 member-symbol
                                 member-name))
    (pushnew (list type-symbol member-symbol member-name)
             (static-members-of (current-context))
             :key #'second)
    member-symbol))

(defun make-type-and-instance-member-symbol (name)
  (let ((symbol (intern name))
        (type-object (handler-case
                         (find-type-from-name
                          name
                          (namespaces-of (current-context)))
                       (clr-type-not-found (x) nil))))
    (when type-object
      ;; If it's also a type, bind it as such.
      (unless (type-symbol-p symbol)
        (bind-type-symbol symbol type-object))
      (pushnew (list symbol (invoke-instance type-object
                                             "AssemblyQualifiedName"))
               (types-of (current-context))
               :key #'first))
    ;; In any case, treat it as a potential instance member symbol.
    (unless (instance-member-symbol-p symbol)
      (bind-instance-member-symbol symbol name))
    (pushnew (list symbol name)
             (instance-members-of (current-context))
             :key #'first)
    symbol))
    
(defun read-clr-symbol (stream &optional char)
  (declare (ignore char))
  (let ((identifier (read-clr-token stream))
        namespace)
    (when (eql #\: (peek-char nil stream nil #\Space t))
      ;; consume peeked char
      (read-char stream t nil t)
      (psetf namespace identifier
             identifier (read-clr-token stream)))
    (multiple-value-bind
          (type-name name) (split-identifier identifier)
      ;; Several possibilities of interest here:
      ;;
      ;; - The name is type-qualified, with or without a namespace.
      ;;   This must be a static member reference.
      ;;
      ;; - The identifier is not type-qualified, and a namespace is
      ;;   specified. This must be a type reference.
      ;; 
      ;; - The identifier is not type-qualified, and and it is not
      ;;   namespace-qualified. This can be a type reference, and/or
      ;;   an instance member reference.
      ;;
      (cond
        (type-name (make-static-member-symbol type-name name namespace))
        (namespace (make-type-symbol identifier namespace))
        (t (make-type-and-instance-member-symbol identifier))))))

;----------------------------------------------------------------------------

(defmacro enable-clr-syntax ()
  "Enable reader syntax and push a new context for it,
initializing the list of namespace to NAMESPACES. (See
USE-NAMESPACES.) If the reader syntax was previously enabled, the
previous list is saved, and is restored after a call to
BIND-CLR-SYMBOLS. A call to USE-NAMESPACES should usually appear
at the top of a file. The file should end with a call to
BIND-CLR-SYMBOLS."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *readtable* (copy-readtable))
     (set-macro-character #\? 'read-clr-symbol t)
     (push-context)
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
         (pushnew ,ns (namespaces-of (current-context)) :test #'equal)
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
       (map nil #'(lambda (args)
                    (bind-type-symbol
                     (first args)
                     (find-type-from-name (second args) nil)))
                     ',(types-of (current-context)))
       (map nil #'(lambda (args)
                    (apply #'bind-static-member-symbol args))
            ',(static-members-of (current-context)))
       (map nil #'(lambda (args)
                    (apply #'bind-instance-member-symbol args))
            ',(instance-members-of (current-context))))
     (eval-when (:compile-toplevel :execute)
       (when ,dump
         (format t "~&The following CLR types were referenced:~%~{  ~A~%~}"
                 (map 'list #'second
                      ',(types-of (current-context))))
         (format t "~&The following CLR static members were referenced:~%~{  ~A~%~}"
                 (map 'list #'(lambda (arg) (symbol-name (second arg)))
                      ',(static-members-of (current-context))))
         (format t "~&The following CLR instance members were referenced:~%~{  ~A~%~}"
                 (map 'list #'second
                      ',(instance-members-of (current-context))))))
     (pop-context)
     (values)))