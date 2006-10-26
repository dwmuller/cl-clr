;;;
;;; Unit tests for argument-based selection of methods and property
;;; accessors.
;;;
(in-package :cl-clr.tests)

(enable-clr-syntax)
(use-namespaces "System"
                "SpookyDistance.CommonLispReflection.TestLibrary")

(deftest member-selection ()
  (let* ((obj1 (new '?OverloadingClass1))
         (object-list '("ArgA" "ArgB"))
         (object-array (list-to-clr-array object-list))
         (int-list '(1 2))
         (int-array (list-to-clr-array int-list
                                       :element-type '?Int32))
         (dclass-list (list (new '?StringEncapsulator "A")
                            (new '?StringEncapsulator "B")))
         (dclass-array
          (list-to-clr-array dclass-list
                             :element-type '?StringEncapsulator)))
    
    (check
      (expect "Selected 0 from (string, string): ArgA"
              (?MC1_Select obj1 0 "ArgA" "ArgB"))
      (expect "Selected 1 from (string, string): ArgB"
              (?MC1_Select obj1 1 "ArgA" "ArgB"))
      (expect "Selected 0 from (string, int): ArgA"
              (?MC1_Select obj1 0 "ArgA" 5))
      (expect "Selected 1 from (string, int): 5"
              (?MC1_Select obj1 1 "ArgA" 5))
      (expect "Selected 0 from (double, int): 5.6"
              (?MC1_Select obj1 0 5.6d0 5))
      (expect "Selected 1 from (double, int): 5"
              (?MC1_Select obj1 1 5.6d0 5))
      
      ;; Let an int match the float parameter:
      (expect "Selected 0 from (double, int): 6"
              (?MC1_Select obj1 0 6 5))
      (expect "Selected 1 from (double, int): 5"
              (?MC1_Select obj1 1 6 5))
      
      ;; Varying params:
      (expect "Selected 0 from (params object[]): ArgA"
              (?MC1_Select obj1 0 "ArgA" "ArgB" "ArgC"))
      (expect "Selected 1 from (params object[]): ArgB"
              (?MC1_Select obj1 1 "ArgA" "ArgB" "ArgC"))
      (expect "Selected 2 from (params object[]): ArgC"
              (?MC1_Select obj1 2 "ArgA" "ArgB" "ArgC"))
      (expect "Selected 1 from (params int[]): 2"
              (?MC1_Select obj1 1 1 2 3))
      (expect "Selected 0 from (params int[]): 1"
              (?MC1_Select obj1 0 1))
      
      ;; Forced use of varying params, from list/array of Object:
      (expect "Selected 0 from (params object[]): ArgA"
              (?MC1_Select obj1 0 (as-var-args object-list)))
      (expect "Selected 1 from (params object[]): ArgB"
              (?MC1_Select obj1 1 (as-var-args object-list)))
      (expect "Selected 0 from (params object[]): ArgA"
              (?MC1_Select obj1 0 (as-var-args object-array)))
      (expect "Selected 1 from (params object[]): ArgB"
              (?MC1_Select obj1 1 (as-var-args object-array)))
      
      ;; Forced use of varying params, from list/array of Int32:
      (expect "Selected 0 from (params int[]): 1"
              (?MC1_Select obj1 0 (as-var-args int-list
                                               :element-type '?Int32)))
      (expect "Selected 1 from (params int[]): 2"
              (?MC1_Select obj1 1 (as-var-args int-list
                                               :element-type '?Int32)))
      (expect "Selected 0 from (params int[]): 1"
              (?MC1_Select obj1 0 (as-var-args int-array)))
      (expect "Selected 1 from (params int[]): 2"
              (?MC1_Select obj1 1 (as-var-args int-array)))

      ;; Forced use of varying params, from array of StringEncapsulator.
      ;; Tests conformance of array to StringEncapsulator[].
      (expect "Selected 0 from (params StringEncapsulator[]): A"
              (?MC1_Select obj1 0
                           (as-var-args dclass-list
                                        :element-type '?StringEncapsulator)))
      (expect "Selected 1 from (params StringEncapsulator[]): B"
              (?MC1_Select obj1 1
                           (as-var-args dclass-list
                                        :element-type '?StringEncapsulator)))
      (expect "Selected 0 from (params StringEncapsulator[]): A"
              (?MC1_Select obj1 0 (as-var-args dclass-array)))
      (expect "Selected 1 from (params StringEncapsulator[]): B"
              (?MC1_Select obj1 1 (as-var-args dclass-array)))

      ;; Test treatment of NIL and T as booleans.
      (?MC2_Bool obj1 t)
      (not (?MC2_Bool obj1 nil))
      
      )))

(bind-clr-symbols)
