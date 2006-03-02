;;;
;;; Functions to manage the list of used namespaces.
;;;
;;; The used namespace list is maintained per-package. These lists
;;; affect the operation of the reader.
;;;
;;; Note: UNUSE-NAMESPACE does not attempt to remove type or member
;;; symbols from the target package. Member symbols would be difficult
;;; to do, but types would easily be possible. Should it do so? This
;;; would be more analogous to UNUSE-PACKAGE.
;;;

(in-package :cl-clr)

(defvar *namespace-hash* (make-hash-table)
  "Mapping of package to namespaces used in that package.")

(defun namespaces-used-by-package (&optional (package *package*))
  "Returns the list of namespaces being used by PACKAGE, which
defaults to *PACKAGE*. The list of used namespaces only affects
the CL-CLR reader."
  (gethash (find-package package) *namespace-hash*))

(defun %use-namespace (namespace &optional (package *package*))
  (setf package (find-package package))
  (pushnew namespace
           (gethash package *namespace-hash*)
           :test #'equal))

(defun %unuse-namespace (namespace &optional (package *package*))
  (setf package (find-package package))
  (setf (gethash package *namespace-hash*)
        (remove namespace (gethash package *namespace-hash*) :test #'equal)))

(defun %unuse-all-namespaces (&optional (package *package*))
  (setf package (find-package package))
  (setf (gethash package *namespace-hash*) nil))

(defmacro use-namespace (namespace &optional (package *package*))
  "Adds a namespace name to the list of namespaces being
used by PACKAGE, which defaults to *PACKAGE*. The list of used
namespaces only affects the CL-CLR reader. Returns the new list."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%use-namespace ,namespace ,package)))

(defmacro unuse-namespace (namespace &optional (package *package*))
  "Removes a namespace name from the list of namespaces being
used by PACKAGE, which defaults to *PACKAGE*. The list of used
namespaces only affects the CL-CLR reader. Returns the new list."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%unuse-namespace ,namespace ,package)))

(defmacro unuse-all-namespaces (&optional (package *package*))
  "Removes all namespace names from the list of namespaces being
used by PACKAGE, which defaults to *PACKAGE*. The list of used
namespaces only affects the CL-CLR reader. Returns the new list."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%unuse-all-namespace ,package)))
