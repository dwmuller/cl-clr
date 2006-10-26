;;;
;;; Unit tests that excercise only classes in the CLR runtime.
;;;
(in-package :cl-clr.tests)

(enable-clr-syntax)
(use-namespaces "System")

(deftest clr-runtime ()
  (let ((appdomain (?AppDomain.CurrentDomain)))
    (check
     ;; constructors
     (expect "System.Byte" (?ToString (?GetType (new '?Byte))))
     
     ;; properties
     (expect "DefaultDomain" (?FriendlyName appdomain))
     
     ;; fields

     ;; nullary methods
     (expect "System.AppDomain" (?FullName (?GetType appdomain))))))

(bind-clr-symbols)