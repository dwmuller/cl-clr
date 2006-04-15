(in-package :cl-clr.tests)

(enable-clr-syntax "System"
                   "SpookyDistance.CommonLispReflection.TestLibrary")

;; From the Lisp FAQ, we get a way to create a "/dev/null" stream:
(defparameter *dev-null*
  #-lispm
  (make-two-way-stream (make-concatenated-stream) (make-broadcast-stream))
  ;; Since Lisp Machines have a built-in /dev/null which handles
  ;; additional, non-standard operations, we'll use that instead.
  #+lispm #'system:null-stream)

(defun crazy ()
  (loop
     for n = (cl-clr::number-of-unreleased-handles)
     do (format t "~A, ~A~%" n (uncollected-object-count))
       (loop
          for i from 1 upto 1000
          do (new '?OverloadingClass1))
;;        (let ((*standard-output* *dev-null*))
;;           (all))
       #+lispworks (hcl:gc-if-needed t)
       (when (< (cl-clr::number-of-unreleased-handles) n)
         (print "Ding! Unreleased handle count decreased. Phew!")
         (return-from crazy))))
