(in-package :cl-user)

(defpackage :common-lisp-common-language-runtime
  (:nicknames :cl-clr)
  (:use :common-lisp :rdnzl)
  
  ;; Things re-implemented here that take precedence over RDNZL's
  ;; versions:
  (:shadow :import-type
           :load-assembly
           :new
           :unuse-all-namespaces
           :unuse-namespace
           :use-namespace
           )
  (:export :bind-namespace
           :def-namespaces
           :do-assemblies
           :find-type-from-namespace-qualified-name
           :find-type-from-name
           :find-type-from-simple-name
           :get-member-symbol
           :get-namespace-package
           :get-type-symbol
           :get-type-object
           :import-type
           :is-namespace-qualified-type-name
           :init-clr
           :load-assembly
           :namespaces-used-by-package
           :new
           :shutdown-clr

           ;; Related to the alternative reader:
           :disable-clr-syntax
           :enable-clr-syntax
           :unuse-all-namespaces
           :unuse-namespace
           :use-namespace
           
           ;; RDNZL symbols re-exported
           :*coerce-double-floats-to-single*
           :aref*
           :box
           :cast
           :container-p
           :define-rdnzl-call
           :disable-rdnzl-syntax
           :do-rdnzl-array
           :enable-rdnzl-syntax
           :enum-to-integer
           :field
           ;:import-assembly
           ;:import-type
           ;:import-types
           :integer-to-enum
           :invoke
           ;:init-rdnzl
           ;:load-assembly
           :list-to-rdnzl-array
           :make-null-object
           ;:new
           :or-enums
           :property
           :ref
           :rdnzl-array-to-list
           :rdnzl-error
           :rdnzl-error-exception
           :rdnzl-handler-case
           ;:shutdown-rdnzl
           :unbox
           ;:unuse-all-namespaces
           ;:unuse-namespace
           ;:use-namespace
           ))

(defpackage :clr-symbols)
