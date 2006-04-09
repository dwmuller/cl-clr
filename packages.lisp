;;; $Id$
;;;
;;; Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.
;;;
(in-package :cl-user)

(defpackage :common-lisp-common-language-runtime
  (:nicknames :cl-clr)
  (:use :common-lisp :cffi)
  
  (:export :aref*
           :bind-namespace
           :box
           :clr-array-to-list
           :clr-object
           :clr-object-p
           :clr-type-name-to-symbol
           :clr-type-of
           ;:define-clr-call TODO
           :do-assemblies
           :do-clr-array
           :enum-value
           :exception-of
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
           :unbox

           ;; Related to the alternative reader:
           :enable-clr-syntax
           :use-namespace
           :use-namespaces
           :bind-clr-symbols
           
           ;; RDNZL symbols that we might like to re-implement
;;            :cast
;;            ;:import-assembly
;;            ;:import-types

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
