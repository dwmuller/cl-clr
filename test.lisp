;;
;; Tests for CL-CLR.
;;
;;
(in-package :cl-user)
(use-package :cl-clr)

(use-namespace "System")

(enable-clr-syntax)

(defun print-assemblies ()
  (let ((app (?CurrentDomain '?+AppDomain))) ;static prop
    (format t "~&Loaded assemblies:")
    (do-rdnzl-array (assy (?GetAssemblies app)) ;instance method
      (format t "~&  ~A" (?FullName (?GetName assy))))))

(defun clr-test1 ()
  (let ((obj1 (get-clr-type-object '?+Type))   ;type
        (obj2 (?+Type?GetType "System.Type"))  ;static method, short form
        (obj3 (?GetType '?+Type "System.Type"))) ;long form
    (assert (and (equal (?FullName obj1)   ;instance prop
                        (?FullName obj2))
                 (equal (?FullName obj2)
                        (?FullName obj3)))))
  (print (?FullName (?GetType (get-clr-type-object '?+Type))))
  (print-assemblies)
  (values))

(disable-clr-syntax)
