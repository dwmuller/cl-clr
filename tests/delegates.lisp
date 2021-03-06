;;;
;;; Unit tests for delegates that call back to Lisp.
;;;
(in-package :cl-clr.tests)

(enable-clr-syntax)
(use-namespaces "System"
                "SpookyDistance.CommonLispReflection.TestLibrary")


(defmacro returns-void (form)
  `(null (multiple-value-list ,form)))

(deftest delegates ()
  (let ((obj1 (new '?EventGeneratorClass1)))

    (let (sentinel)
      (check

       (returns-void (?add_DoSomethingFromNothing
                      obj1
                      (new '?DoSomethingFromNothingDelegate
                           #'(lambda () (setf sentinel t)))))
       (returns-void (?RaiseDoSomethingFromNothing obj1))
       sentinel))

    (let (sentinel)
      (check
       (returns-void (?add_DoSomethingFromObject
                      obj1
                      (new '?DoSomethingFromObjectDelegate
                           #'(lambda (i) (setf sentinel i)))))
       (returns-void (?RaiseDoSomethingFromObject obj1 1))
       (eql sentinel 1)
       (returns-void (?RaiseDoSomethingFromObject obj1 5))
       (eql sentinel 5)
       (returns-void (?RaiseDoSomethingFromObject obj1 "A String"))
       (expect "A String" sentinel)))
       
    (check
     (returns-void (?add_ReturnIntFromNothing
                    obj1
                    (new '?ReturnIntFromNothingDelegate
                         #'(lambda () 36))))
       (eql (?RaiseReturnIntFromNothing obj1) 36))

    ;; Test ability to return ref-type objects from delegates.
    ;; Also tests handle management when a delegate returns
    ;; an object that has continued existence -- in this case,
    ;; an object that was handed in as an argument.
    (let ((arg1 (new '?StringEncapsulator "A"))
          (arg2 (new '?StringEncapsulator "B")))
      (check
        (returns-void (?add_ReturnObjectFromArgs
                       obj1
                       (new '?ReturnObjectFromArgsDelegate
                            #'(lambda (value_arg ref_arg1 ref_arg2)
                                (if (zerop value_arg)
                                    ref_arg1
                                    ref_arg2)))))
        (expect "A" (?ToString (?RaiseReturnObjectFromArgs obj1 0 arg1 arg2)))
        (expect "B" (?ToString (?RaiseReturnObjectFromArgs obj1 1 arg1 arg2)))
        (zerop (unwrapped-handle-count))))
    
    ;; TODO: Add negative tests, removal tests, multicasting tests.
    ))
     
(bind-clr-symbols)