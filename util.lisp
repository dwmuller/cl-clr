
(in-package :cl-clr)

(use-namespaces "System"
                "System.Reflection")

(defun print-members (type member-name)
  (let ((members-info
         (?.GetMember type
                      member-name
                      (or-enums
                       (binding-flag "Static")
                       (binding-flag   "Public")
                       (binding-flag "FlattenHierarchy")))))
    (do-rdnzl-array (member-info members-info)
      (format t "~&~S: ~A ~A "
              (?.Name member-info)
              (?.Name (?.DeclaringType member-info))
              (?.Name (?.ReflectedType member-info))))))

(defun print-types (prefix &optional full)
  (do-rdnzl-array (assembly
                   (?.GetAssemblies (?.CurrentDomain '?System.AppDomain)))
      (do-rdnzl-array (type-object (?.GetExportedTypes assembly))
        (when (zerop (or (search prefix (?.FullName type-object)) 1))
              (format t "~&~A"
                      (if full
                          (?.AssemblyQualifiedName type-object)
                          (?.FullName type-object)))))))

(bind-clr-symbols)