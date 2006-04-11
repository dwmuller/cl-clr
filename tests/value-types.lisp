;;;
;;; Unit tests that excercise value types -- enums, structs.
;;;
(in-package :cl-clr.tests)

(enable-clr-syntax "System"
                   "System.Reflection"
                   "SpookyDistance.CommonLispReflection.TestLibrary")


(deftest value-types ()
  (check
    (new '?Struct1)
    (new '?Struct2)
    (new '?Struct2 5)
    
    (let ((public (enum-value '?BindingFlags '?.Public))
          (static (enum-value '?BindingFlags "Static")))
      (eql (or-enum-values '?BindingFlags "Public" "Static")
           (or-enum-values '?BindingFlags public static)))))
       
(bind-clr-symbols)