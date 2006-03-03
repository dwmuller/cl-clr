;;
;; Tests for CL-CLR.
;;
;;

(in-package :cl-user)
(use-package :cl-clr)

(use-namespace "System")

(enable-clr-syntax)

(defun print-assemblies ()
  (let ((app (?.CurrentDomain '?AppDomain))) ;static prop
    (format t "~&Loaded assemblies:")
    (do-rdnzl-array (assy (?.GetAssemblies app)) ;instance method
      (format t "~&  ~A" (?.FullName (?.GetName assy))))))

(defun clr-test1 ()
  (let ((obj1 (get-type-object "System.Type"))   ;type
        (obj2 (?.GetType '?Type "System.Type")))  ;static method
    (assert (equal (?.FullName obj1)   ;instance prop
                   (?.FullName obj2))))
  (print (?.FullName (?.GetType (get-type-object "System.Type"))))
  (print-assemblies)
  (values))

(disable-clr-syntax)

(clr-test1)
(shutdown-clr)
(init-clr)
(clr-test1)
