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
                                     "ffi"
                                     "types"
                                     "invoke"))
                 (:file "init"
                        :depends-on ("packages"
                                     "invoke"
                                     "symbols"))
                 (:file "assemblies"
                        :depends-on ("packages"))
                 (:file "reader"
                        :depends-on ("packages"
                                     "symbols"))
                 (:file "enums"
                        :depends-on ("packages"
                                     "ffi"
                                     "invoke"
                                     "symbols"))
                 ;; Util depends on init because it
                 ;; runs functions in reader.
                 (:file "util"
                        :depends-on ("packages"
                                     "ffi"
                                     "invoke"
                                     "reader"
                                     "init"))
                 (:file "define"
                        :depends-on ("packages"
                                     "ffi"
                                     "invoke"
                                     "util"))
                 (:system "cffi"))
    :depends-on ("cffi"))
