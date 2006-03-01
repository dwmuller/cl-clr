(defun print-members (type member-name)
  (let ((members-info (invoke type
                              "GetMember"
                              member-name
                              (or-similar-enums "System.Reflection.BindingFlags"
                                                "Static"
                                                "Public"
                                                "FlattenHierarchy"))))
    (do-rdnzl-array (member-info members-info)
      (format t "~&~S: ~A ~A "
              (property member-info "Name")
              (property (property member-info "DeclaringType") "Name")
              (property (property member-info "ReflectedType") "Name")))))

