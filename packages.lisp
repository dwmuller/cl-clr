;;; $Id$
;;;
;;; Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.
;;;
(in-package :cl-user)

(defpackage :common-lisp-common-language-runtime
  (:nicknames :cl-clr)
  (:use :common-lisp :cffi)
  
  (:export :aref*
           :as-var-args
           :bind-namespace
           :clr-array-to-list
           :clr-object
           :clr-object-p
           :clr-type-name-to-symbol
           :clr-type-of
           ;:define-clr-call TODO
           :do-assemblies
           :do-clr-array
           :enum-value
           :import-type
           :init-clr
           :list-to-clr-array
           :load-assembly
           :new
           :or-enum-values
           :print-members
           :print-types
           :ref
           :shutdown-clr

           ;; Related to the alternative reader:
           :enable-clr-syntax
           :use-namespace
           :use-namespaces
           :bind-clr-symbols

           ;; For debugging and metrics:
           :unwrapped-handle-count
           :unreleased-handle-count
           :uncollected-object-count
           
           ;; RDNZL symbols that we might like to re-implement
;;            :cast
;;            ;:import-assembly
;;            ;:import-types
;;            ;:make-null-reference or something like it -- no type needed.

;;            :field           
;;            :invoke
;;            :property
           
;;            :rdnzl-error
;;            :rdnzl-error-exception
;;            :rdnzl-handler-case
;;            ;:unuse-all-namespaces
;;            ;:unuse-namespace
           ))

(defpackage :clr-symbols)
