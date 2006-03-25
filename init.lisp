;;; $Header:$
;;;
;;; Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.
;;;
(in-package :cl-clr)

(defun init-clr ()
  "Initializes CLCLR, and loads objects previously referenced by
the system."
  (init-rdnzl)
  (init-symbols)
  (values))

(defun shutdown-clr ()
  "Releases all CLR resources held by the CLCLR. A call to
INIT-CLR can reconstitute the current environment if the
necessary assemblies are already loaded and define the necessary
types and members."
  (release-symbols)
  (shutdown-rdnzl)
  (values))

(eval-when (:load-toplevel :execute)
  (init-clr))
