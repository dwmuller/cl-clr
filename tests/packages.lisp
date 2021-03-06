;;; $Id:$
;;;
;;; Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.
;;;
(in-package :cl-user)

(defpackage :pcl-unit-test-framework
  (:use :common-lisp)
  (:export :check
           :deftest
           :expect
           :report-result))

(defpackage :common-lisp-common-language-runtime.tests
  (:nicknames :cl-clr.tests)
  (:use :common-lisp :cl-clr :pcl-unit-test-framework)
  (:export :test-clr-runtime))

(cl-clr:load-assembly "SpookyDistance.CommonLispReflection.TestLibrary")
