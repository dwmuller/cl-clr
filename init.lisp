;;; $Id$
;;;
;;; Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.
;;;
(in-package :cl-clr)

(defun init-clr ()
  "Initializes CLCLR, and loads objects previously referenced by
the system."
  (init-ffi)
  (init-symbols)
  (values))

(defun shutdown-clr ()
  "Releases all CLR resources held by the CLCLR. A call to
INIT-CLR can reconstitute the current environment if the
necessary assemblies are already loaded and define the necessary
types and members."
  (shutdown-symbols)
  (shutdown-ffi)
  (values))

(eval-when (:load-toplevel :execute)
  (init-clr))
