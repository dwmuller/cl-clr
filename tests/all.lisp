(in-package :cl-clr.tests)

(deftest all ()
  (check
    (value-types)
    (member-access)
    (member-selection)))
