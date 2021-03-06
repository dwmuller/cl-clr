;;;
;;; Unit tests that test basic access to trivial members, involving
;;; inheritance and interface implementation.
;;;
(in-package :cl-clr.tests)

(enable-clr-syntax)
(use-namespaces "System"
                "SpookyDistance.CommonLispReflection.TestLibrary")

;;; In the test classes, member name prefixes indicate some
;;; characteristics of their first definition:
;;;
;;; (Method|Property|Field)(Class|Interface)(Static|Virtual|Abstract)?[0-9]+
;;;
;;; Abbreviated:
;;; (M|P|F)(C|I)(S|V|A)?[0-9]+
;;;
(deftest member-access ()
  (let ((obj1 (new '?ConcreteClass1))
        (obj2 (new '?TrivialDerivedClass1))
        (obj3 (new '?OverridingClass1))
        (obj4 (new '?ShadowingClass1))
        (obj5 (new '?InterfaceImplementorClass1))
        (obj6 (new '?MultipleInterfaceImplementorClass1)))
    (check

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Defining class
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Invoking regular instance members via defining class instance:
      (expect "ConcreteClass1.MC1()" (?MC1 obj1))
      (expect "ConcreteClass1.PC1" (?PC1 obj1))
      (expect "ConcreteClass1.FC1" (?FC1 obj1))
      ;; Void return type returns no args.
      (eq nil (multiple-value-list (?MC2 obj1)))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Invoking static members via defining class:
      (expect "ConcreteClass1.MCS1()" (?ConcreteClass1.MCS1))
      (expect "ConcreteClass1.PCS1" (?ConcreteClass1.PCS1))
      (expect "ConcreteClass1.FCS1" (?ConcreteClass1.FCS1))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Invoking virtual members via defining class instance:
      (expect "ConcreteClass1.MCV1()" (?MCV1 obj1))
      (expect "ConcreteClass1.PCV1" (?PCV1 obj1))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Setting regular members via defining class instance:
      (setf (?PC1 obj1) "ConcreteClass1.PC1.new")
      (expect "ConcreteClass1.PC1.new" (?PC1 obj1))
      (setf (?FC1 obj1) "ConcreteClass1.FC1.new")
      (expect "ConcreteClass1.FC1.new" (?FC1 obj1))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Setting virtual members via defining class instance
      (setf (?PCV1 obj1) "ConcreteClass1.PCV1.new")
      (expect "ConcreteClass1.PCV1.new" (?PCV1 obj1))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Derived class
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Invoking regular instance members via derived class instance:
      (expect "ConcreteClass1.MC1()" (?MC1 obj2))
      (expect "ConcreteClass1.PC1" (?PC1 obj2))
      (expect "ConcreteClass1.FC1" (?FC1 obj2))
      ;; Void return type returns no args.
      (eq nil (multiple-value-list (?MC2 obj2)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Invoking static members via derived class:
      (expect "ConcreteClass1.MCS1()" (?TrivialDerivedClass1.MCS1))
      (expect "ConcreteClass1.PCS1" (?TrivialDerivedClass1.PCS1))
      (expect "ConcreteClass1.FCS1" (?TrivialDerivedClass1.FCS1))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Invoking virtual members via non-overriding class instance:
      (expect "ConcreteClass1.MCV1()" (?MCV1 obj2))
      (expect "ConcreteClass1.PCV1" (?PCV1 obj2))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Setting regular members via non-overridiing class instance:
      (setf (?PC1 obj2) "TrivialDerivedClass1.PC1.new")
      (expect "TrivialDerivedClass1.PC1.new" (?PC1 obj2))
      (setf (?FC1 obj2) "TrivialDerivedClass1.FC1.new")
      (expect "TrivialDerivedClass1.FC1.new" (?FC1 obj2))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Setting virtual members via non-overriding class instance
      (setf (?PCV1 obj2) "TrivialDerivedClass1.PCV1.new")
      (expect "TrivialDerivedClass1.PCV1.new" (?PCV1 obj2))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Overriding class
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Invoking virtual members via overriding class instance:
      (expect "OverridingClass1.MCV1()" (?MCV1 obj3))
      (expect "OverridingClass1.PCV1" (?PCV1 obj3))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Setting regular members via overridiing class instance:
      (setf (?PC1 obj3) "OverridingClass1.PC1.new")
      (expect "OverridingClass1.PC1.new" (?PC1 obj3))
      (setf (?FC1 obj3) "OverridingClass1.FC1.new")
      (expect "OverridingClass1.FC1.new" (?FC1 obj3))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Setting virtual members via overriding class instance
      (setf (?PCV1 obj3) "OverridingClass1.PCV1.new")
      (expect "OverridingClass1.PCV1.new" (?PCV1 obj3))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Shadowing class
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Invoking regular instance members via shadowing class instance:
      (expect "ShadowingClass1.MC1()" (?MC1 obj4))
      (expect "ShadowingClass1.PC1" (?PC1 obj4))
      (expect "ShadowingClass1.FC1" (?FC1 obj4))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Invoking static members via shadowing class:
      (expect "ShadowingClass1.MCS1()" (?ShadowingClass1.MCS1))
      (expect "ShadowingClass1.PCS1" (?ShadowingClass1.PCS1))
      (expect "ShadowingClass1.FCS1" (?ShadowingClass1.FCS1))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Invoking virtual members via shadowing class instance:
      (expect "ShadowingClass1.MCV1()" (?MCV1 obj4))
      (expect "ShadowingClass1.PCV1" (?PCV1 obj4))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Setting virtual members via shadowing class instance
      (setf (?PCV1 obj2) "ShadowingClass1.PCV1.new")
      (expect "ShadowingClass1.PCV1.new" (?PCV1 obj2))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Interface implementation

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Simple interface implementation
      (expect "InterfaceImplementorClass1.MI1()" (?MI1 obj5))
      (expect "InterfaceImplementorClass1.PI1"   (?PI1 obj5))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Multiple interface implementation with similar mebmer names.
      (expect "MultipleInterfaceImplementorClass1.MI1()" (?MI1 obj6))
      (expect "MultipleInterfaceImplementorClass1.PI1"   (?PI1 obj6))
      
      )))

(bind-clr-symbols t)