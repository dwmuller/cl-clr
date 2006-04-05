;;;
;;; Unit tests that excercise only classes in the CLR runtime.
;;;
(in-package :cl-clr.tests)

(enable-clr-syntax "System")

(deftest test-clr-runtime ()
  (let ((appdomain (?.CurrentDomain '?AppDomain)))
    (check
     ;; constructors
     (equal (?.ToString (?.GetType (new '?Byte))) "System.Byte")
     
     ;; properties
     (equal (?.FriendlyName appdomain) "DefaultDomain")
     
     ;; fields

     ;; nullary methods
     (equal (?.FullName (?.GetType appdomain)) "System.AppDomain"))))

(bind-clr-symbols)