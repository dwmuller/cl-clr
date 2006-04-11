;;;
;;; Unit tests for argument-based selection of methods and property
;;; accessors.
;;;
(in-package :cl-clr.tests)

(enable-clr-syntax "System"
                   "SpookyDistance.CommonLispReflection.TestLibrary")

(deftest member-selection ()
  (let* ((obj1 (new '?OverloadingClass1))
         (object-list '("ArgA" "ArgB"))
         (object-array (list-to-clr-array object-list))
         (int-list '(1 2))
         (int-array (list-to-clr-array int-list
                                       :element-type '?Int32))
         (dclass-list (list (new '?TrivialDerivedClass1 "A")
                            (new '?TrivialDerivedClass1 "B")))
         (dclass-array
          (list-to-clr-array dclass-list
                             :element-type '?TrivialDerivedClass1)))
    
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
      
      ;; Forced use of varying params, from list/array of Object:
      (equal "Selected 0 from (params object[]): ArgA"
             (?.MC1_Select obj1 0 (as-var-args object-list)))
      (equal "Selected 1 from (params object[]): ArgB"
             (?.MC1_Select obj1 1 (as-var-args object-list)))
      (equal "Selected 0 from (params object[]): ArgA"
             (?.MC1_Select obj1 0 (as-var-args object-array)))
      (equal "Selected 1 from (params object[]): ArgB"
             (?.MC1_Select obj1 1 (as-var-args object-array)))
      
      ;; Forced use of varying params, from list/array of Int32:
      (equal "Selected 0 from (params int[]): 1"
             (?.MC1_Select obj1 0 (as-var-args int-list
                                               :element-type '?Int32)))
      (equal "Selected 1 from (params int[]): 2"
             (?.MC1_Select obj1 1 (as-var-args int-list
                                               :element-type '?Int32)))
      (equal "Selected 0 from (params int[]): 1"
             (?.MC1_Select obj1 0 (as-var-args int-array)))
      (equal "Selected 1 from (params int[]): 2"
             (?.MC1_Select obj1 1 (as-var-args int-array)))

      ;; Forced use of varying params, from array of TrivialDerivedClass1.
      ;; Tests conformance of array to ConcreteClass1[].
      (equal "Selected 0 from (params ConcreteClass1[]): A"
             (?.MC1_Select obj1 0
                           (as-var-args dclass-list
                                        :element-type '?TrivialDerivedClass1)))
      (equal "Selected 1 from (params ConcreteClass1[]): B"
             (?.MC1_Select obj1 1
                           (as-var-args dclass-list
                                        :element-type '?TrivialDerivedClass1)))
      (equal "Selected 0 from (params ConcreteClass1[]): A"
             (?.MC1_Select obj1 0 (as-var-args dclass-array)))
      (equal "Selected 1 from (params ConcreteClass1[]): B"
             (?.MC1_Select obj1 1 (as-var-args dclass-array)))

      ;; Test treatment of NIL and T as booleans.
      (?.MC2_Bool obj1 t)
      (not (?.MC2_Bool obj1 nil))
      
      )))

(bind-clr-symbols)