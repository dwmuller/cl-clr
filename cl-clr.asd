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
                 (:file "namespaces"
                        :depends-on ("packages"))
                 (:file "symbols"
                        :depends-on ("packages"
                                     "types"))
                 (:file "init"
                        :depends-on ("packages"
                                     "symbols"))
                 (:file "types"
                        :depends-on ("packages"
                                     "namespaces"))
                 (:file "reader"
                        :depends-on ("packages"
                                     "symbols"
                                     "types"
                                     "namespaces"))
                 (:file "assemblies"
                        :depends-on ("packages"
                                     "reader"))
                 (:file "imports"
                        :depends-on ("packages"
                                     "symbols"
                                     "reader"
                                     "assemblies"))
                 (:system "rdnzl"))
    :depends-on ("rdnzl"))
