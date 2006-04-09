;;;
;;; Unit tests for argument-based selection of methods and property
;;; accessors.
;;;
(in-package :cl-clr.tests)

(enable-clr-syntax "System"
                   "SpookyDistance.CommonLispReflection.TestLibrary")

(defun test-member-selection ()
  (let* ((obj1 (new '?OverloadingClass1))
         (var-args-list '("ArgA" "ArgB")))
    
    (check
      (equal "Selected 0 from (string, string): ArgA"
             (?.MC1_Select obj1 0 "ArgA" "ArgB"))
      (equal "Selected 1 from (string, string): ArgB"
             (?.MC1_Select obj1 1 "ArgA" "ArgB"))
      (equal "Selected 0 from (string, int): ArgA"
             (?.MC1_Select obj1 0 "ArgA" 5))
      (equal "Selected 1 from (string, int): 5"
             (?.MC1_Select obj1 1 "ArgA" 5))
      (equal "Selected 0 from (double, int): 5.6"
             (?.MC1_Select obj1 0 5.6d0 5))
      (equal "Selected 1 from (double, int): 5"
             (?.MC1_Select obj1 1 5.6d0 5))
      
      ;; Let an int match the float parameter:
      (equal "Selected 0 from (double, int): 6"
             (?.MC1_Select obj1 0 6 5))
      (equal "Selected 1 from (double, int): 5"
             (?.MC1_Select obj1 1 6 5))
      
      ;; Varying params:
      (equal "Selected 0 from (params object[]): ArgA"
             (?.MC1_Select obj1 0 "ArgA" "ArgB" "ArgC"))
      (equal "Selected 1 from (params object[]): ArgB"
             (?.MC1_Select obj1 1 "ArgA" "ArgB" "ArgC"))
      (equal "Selected 2 from (params object[]): ArgC"
             (?.MC1_Select obj1 2 "ArgA" "ArgB" "ArgC"))
      
      ;; Forced use of varying params, from list:
      (equal "Selected 0 from (params object[]): ArgA"
             (?.MC1_Select obj1 0 (as-var-args var-args-list)))
      (equal "Selected 1 from (params object[]): ArgB"
             (?.MC1_Select obj1 1 (as-var-args var-args-list)))

    )))
