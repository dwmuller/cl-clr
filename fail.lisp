(in-package :cl-user)
(use-package :cl-clr)

(import-assembly "System.Windows.Forms")

;; This fails, because the system doesn't find the type.
;; Comment it out and try reloading to move on to the next test.
;(new "System.Windows.Forms.Form")

;; This fails, too. Same problem.
;; Comment it out and try reloading to move on to the next test.
;(import-type "System.Windows.Forms.Form")

;; I also tried direct calls to System.Type.GetType with various
;; abbreviations of the fully qualified type name, leaving off
;; trailing pieces of the full assembly name. None of these succeeded.

;; This succeeds.
(import-type "System.Windows.Forms.Form"
            (load-assembly "System.Windows.Forms"))

;; This succeeds, because RDNZL now remembers what assembly it found
;; Form in.
(let ((form
      ;; This fails, because the system doesn't find the type.
      (new "System.Windows.Forms.Form")))
 (invoke form "Dispose"))
