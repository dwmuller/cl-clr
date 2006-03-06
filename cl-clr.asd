;;;
;;; This file provides ASDF definitions for Common Lisp Common
;;; Language Runtime.
;;;

(in-package :common-lisp-user)

;;;
;;; ASDF definitions are conventionally kept in their own package.
;;;
(defpackage :cl-clr-system
  (:use :cl :asdf))
(in-package :cl-clr-system)

(asdf:defsystem :cl-clr
    :components ((:file "packages")
                 (:file "types"
                        :depends-on ("packages"))
                 (:file "symbols"
                        :depends-on ("packages"
                                     "types"))
                 (:file "init"
                        :depends-on ("packages"
                                     "symbols"))
                 (:file "assemblies"
                        :depends-on ("packages"))
                 (:file "reader"
                        :depends-on ("packages"
                                     "symbols"
                                     "types"))
                 (:system "rdnzl"))
    :depends-on ("rdnzl"))
