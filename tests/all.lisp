(in-package :cl-clr.tests)

(deftest all ()
  (check
    (value-types)
    (zerop (unwrapped-handle-count))
    (member-access)
    (zerop (unwrapped-handle-count))
    (member-selection)
    (zerop (unwrapped-handle-count))
    (delegates)
    (zerop (unwrapped-handle-count))))

