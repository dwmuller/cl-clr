(in-package :cl-clr.tests)

(deftest all ()
  (check
    (value-types)
    (member-access)
    (member-selection)
    (zerop (cl-clr::number-of-unreleased-handles))))


(defun crazy ()
  (loop
     for n = (cl-clr::number-of-unreleased-handles)
     do (print n)
       (let ((*standard-output* *dev-null*))
          (all))
       (if (< (cl-clr::number-of-unreleased-handles) n)
           (print "Ding!"))))
