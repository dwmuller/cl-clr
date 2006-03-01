(in-package :cl-clr)

(defvar *namespace-hash* (make-hash-table)
  "Mapping of package to namespaces used in that package.")

(defun namespaces-used-by-package (&optional (package *package*))
  "Returns the list of namespaces being used by PACKAGE, which
defaults to *PACKAGE*. The list of used namespaces only affects
the CL-CLR reader."
  (gethash (find-package package) *namespace-hash*))

(defun %use-namespace (namespace &optional (package *package*))
  "Removes a namespace name from the list of namespaces being
used by PACKAGE, which defaults to *PACKAGE*. The list of used
namespaces only affects the CL-CLR reader. Returns the new list."
  (setf package (find-package package))
  (pushnew namespace
           (gethash package *namespace-hash*)
           :test #'equal))

(defun %unuse-namespace (namespace &optional (package *package*))
  "Adds a namespace name to the list of namespaces being used by
PACKAGE, which defaults to *PACKAGE*. The list of used namespaces
only affects the CL-CLR reader. Returns the new list."
  (setf package (find-package package))
  (setf (gethash package *namespace-hash*)
        (remove namespace (gethash package *namespace-hash*) :test #'equal)))

(defmacro use-namespace (namespace &optional (package *package*))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%use-namespace ,namespace ,package)))

(defmacro unuse-namespace (namespace &optional (package *package*))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%unuse-namespace ,namespace ,package)))
