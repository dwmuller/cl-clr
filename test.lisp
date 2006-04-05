;;; $Id$
;;;
;;; Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.
;;
;; Tests for CL-CLR.
;;
;;

(in-package :cl-user)
(use-package :cl-clr)

(enable-clr-syntax "System")

(defun print-assemblies ()
  (let ((app (?.CurrentDomain '?AppDomain))) ;static prop
    (format t "~&Loaded assemblies:")
    (do-clr-array (assy (?.GetAssemblies app)) ;instance method
      (format t "~&  ~A" (?.FullName (?.GetName assy))))))

(defun clr-test1 ()
  (print (?.FullName (?.GetType (?.GetType '?Type "System.Type"))))
  (print-assemblies)
  (values))

(bind-clr-symbols)

(clr-test1)
(shutdown-clr)
(init-clr)
(clr-test1)
