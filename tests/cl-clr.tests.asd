;;; $Id:$
;;;
;;; Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.
;;;
;;; This file provides ASDF definitions for Common Lisp Common
;;; Language Runtime.
;;;

(in-package :common-lisp-user)

(defpackage :cl-clr.tests-system
  (:use :cl :asdf))
(in-package :cl-clr.tests-system)

(asdf:defsystem :cl-clr.tests
    :components ((:file "packages")
                 (:file "pcl-unit-test-framework"
                        :depends-on ("packages"))
                 (:file "clr-runtime"
                        :depends-on ("packages"
                                     "pcl-unit-test-framework"))
                 (:system "cl-clr"))
    :depends-on ("cl-clr"))


