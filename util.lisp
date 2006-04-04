;;; $Id$
;;;
;;; Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.
;;;

(in-package :cl-clr)

(use-namespaces "System"
                "System.Reflection")

(defun print-members (type member-name)
  (let ((members-info
         (?.GetMember type
                      member-name
                      (binding-flag "Static")
                      (binding-flag   "Public")
                      (binding-flag "FlattenHierarchy"))))
    (do-clr-array (member-info members-info)
      (format t "~&~S: ~A ~A "
              (?.Name member-info)
              (?.Name (?.DeclaringType member-info))
              (?.Name (?.ReflectedType member-info))))))

(defun print-types (prefix &optional full)
  (do-clr-array (assembly
                   (?.GetAssemblies (?.CurrentDomain '?System.AppDomain)))
      (do-clr-array (type-object (?.GetExportedTypes assembly))
        (when (zerop (or (search prefix (?.FullName type-object)) 1))
              (format t "~&~A"
                      (if full
                          (?.AssemblyQualifiedName type-object)
                          (?.FullName type-object)))))))

(bind-clr-symbols t)