;;;
;;; Functions for bulk importing CLR symbols that designate types and
;;; members.
;;;
(in-package :cl-clr)

(enable-clr-syntax)

(defun import-type (type &optional (package *package*))
  "TYPE is a symbol or string designating a CLR
type. IMPORT-TYPES imports the symbol designating the CLR type to
PACKAGE, which defaults to *PACKAGE*. It also imports symbols to
represent all public members of the CLR type. Returns the symbol
designating the CLR type."
  (setf package (find-package *package*))
  (ecase type
    (string type (setf type (get-type-symbol type)))
    (symbol (assert (eql (symbol-package type) (find-package :cl-clr)))))
  (import type package)
  (let ((type-object (get type 'type)))
    (do-rdnzl-array (member-info (?GetMembers type-object))
      (import (get-member-symbol (?Name member-info)))))
  type)
  
(defun import-namespace (namespace &optional (package *package*))
  "Imports all type and member symbols of a namespace from all
assemblies in the current application domain to PACKAGE, which
defaults to *PACKAGE*. Returns nothing."
  (setf package (find-package *package*))
  (do-assemblies assembly
    (do-rdnzl-array (type-object (?GetTypes assembly))
      (when (zerop (search namespace (?FullName type-object)))
        (import-type (get-type-symbol type-object) package))))
  (values))

(disable-clr-syntax)