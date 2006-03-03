;;;
;;; Functions for bulk importing CLR symbols that designate types and
;;; members.
;;;
(in-package :cl-clr)

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
    (do-rdnzl-array (member-info (invoke  type-object "GetMembers"))
      (import (get-member-symbol (property member-info "Name")))))
  type)
  
(defun import-namespace (namespace &optional (package *package*))
  "Imports all type and member symbols of a namespace from all
assemblies in the current application domain to PACKAGE, which
defaults to *PACKAGE*. Returns nothing."
  (setf package (find-package *package*))
  (do-assemblies assembly
    (do-rdnzl-array (type-object (invoke assembly "GetTypes"))
      (when (zerop (search namespace (property type-object "FullName")))
        (import-type (get-type-symbol type-object) package))))
  (values))

