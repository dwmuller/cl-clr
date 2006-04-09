(in-package :cl-clr.tests)

(defun test-all ()
  (check
    (test-value-types)
    (test-member-access)
    (test-member-selection)))
