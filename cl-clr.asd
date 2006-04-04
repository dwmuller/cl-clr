;;; $Id$
;;;
;;; Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.
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
                 (:file "ffi"
                        :depends-on ("packages"))
                 (:file "invoke"
                        :depends-on ("packages"
                                     "ffi"))
                 (:file "types"
                        :depends-on ("packages"
                                     "ffi"
                                     "invoke"))
                 (:file "symbols"
                        :depends-on ("packages"
                                     "types"
                                     "invoke"))
                 (:file "init"
                        :depends-on ("packages"
                                     "ffi"
                                     "symbols"))
                 (:file "assemblies"
                        :depends-on ("packages"))
                 (:file "reader"
                        :depends-on ("packages"
                                     "symbols"
                                     "types"))
                 (:file "util"
                        :depends-on ("packages"
                                     "reader"))
                 (:file "enums"
                        :depends-on ("packages"
                                     "ffi"
                                     "invoke"))
                 (:system "rdnzl")
                 (:system "cffi"))
    :depends-on ("rdnzl" "cffi"))
