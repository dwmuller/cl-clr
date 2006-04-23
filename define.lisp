;;; $Id:$
;;;
;;; Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.
;;;

(in-package :cl-clr)

(defun lisp-to-clr-name (name)
  (coerce (loop
             with upcase = t
             for char across name
             unless (eql char #\-) collect (if upcase
                                               (progn (setf upcase nil)
                                                      (char-upcase char))
                                               (char-downcase char))
             when (eql char #\-) do (setf upcase t))
          'string))

(defun clr-to-lisp-name (name)
  (coerce (loop
             with sep = nil
             for char across name
             when (and (upper-case-p char) sep)
             collect #\- and do (setf sep nil)
             when (upper-case-p char) do (setf sep t)
             collect (char-downcase char))
          'string))

(defun make-field-definition-forms (lisp-name fld-info)
  ;; TODO: Could make this more efficient by resolving Set/GetValue.
  ;; ... or by building direct support in substrate?
  (cond ((invoke-instance fld-info "IsStatic")
         `((setf (fdefinition ',lisp-name)
                 #'(lambda ()
                     (invoke-instance (get ',lisp-name 'member-info)
                                      "GetValue"
                                      nil)))
           (setf (fdefinition '(setf ,lisp-name))
                 #'(lambda (new-value)
                     (invoke-instance (get ',lisp-name 'member-info)
                                      "SetValue"
                                      nil
                                      new-value)))))
        (t
         `((setf (fdefinition ',lisp-name)
                 #'(lambda (obj)
                     (invoke-instance
                      (get ',lisp-name 'member-info)
                      "GetValue"
                      obj)))
           (setf (fdefinition '(setf ,lisp-name))
                 #'(lambda (new-value obj)
                     (invoke-instance (get ',lisp-name 'member-info)
                                      "SetValue"
                                      obj
                                      new-value)))))))

(defun arg-types-to-types-array (arg-types)
  (list-to-clr-array (map 'list #'(lambda (arg-type)
                                    (to-clr-type-object arg-type))
                          arg-types) :element-type "System.Type"))

(defun make-property-definition-forms (lisp-name prop-info arg-defs)
  ;; Quite inexplicably, a PropertyInfo object has no IsStatic
  ;; property. So we have to dive into the accessor methods to
  ;; find this critical bit of info.
  (let ((arg-names (map 'list #'car arg-defs))
        (obj-symbol (gensym))
        result)
    (when (invoke-instance prop-info "CanWrite")
      (let ((new-value-symbol (gensym)))
        (push 
         (if (invoke-instance (invoke-instance prop-info "GetSetMethod")
                              "IsStatic")
             `(setf (fdefinition '(setf ,lisp-name))
                    #'(lambda (,new-value-symbol ,@arg-names)
                        (invoke-instance (get ',lisp-name 'member-info)
                                         "SetValue"
                                         nil
                                         ,new-value-symbol
                                         ,@arg-names)))
             `(setf (fdefinition '(setf ,lisp-name))
                    #'(lambda (,new-value-symbol ,obj-symbol ,@arg-names)
                        (invoke-instance (get ',lisp-name 'member-info)
                                         "SetValue"
                                         ,obj-symbol
                                         ,new-value-symbol
                                         ,@arg-names))))
         result)))
    (when (invoke-instance prop-info "CanRead")
      (push
       (if (invoke-instance (invoke-instance prop-info "GetGetMethod")
                            "IsStatic")
           `(setf (fdefinition ',lisp-name)
                  #'(lambda ,arg-names
                      (invoke-instance (get ',lisp-name 'member-info)
                                       "GetValue"
                                       nil
                                       ,@arg-names)))
           `(setf (fdefinition ',lisp-name)
                  #'(lambda (,obj-symbol ,@arg-names)
                      (invoke-instance (get ',lisp-name 'member-info)
                                       "GetValue"
                                       ,obj-symbol
                                       ,@arg-names))))
       result))))

(defun make-method-definition-form (lisp-name method-info arg-defs)
  (let ((arg-names (map 'list #'car arg-defs)))
    (if (invoke-instance method-info "IsStatic")
        `(setf (fdefinition ',lisp-name)
               #'(lambda ,arg-names
                   (invoke-instance (get ',lisp-name 'method-info)
                                    "Invoke"
                                    nil
                                    ,@arg-names)))
        (let ((obj-symbol (gensym)))
          `(setf (fdefinition ',lisp-name)
                 (lambda (,obj-symbol ,@arg-names)
                   (invoke-instance (get ',lisp-name 'method-info)
                                    "Invoke"
                                    ,obj-symbol
                                    ,@arg-names)))))))

(defun get-member-kind (type-object name)
  (let ((members (invoke-instance type-object "GetMember" name)))
    (unless (and members (plusp (invoke-instance members "Length")))
      (error "No member ~S found in type ~S."
             name (invoke-instance type-object "FullName")))
    ;; Assume that all members with this name are of the same
    ;; kind. This will be true for CLS-compliant types.
    (invoke-instance (aref* members 0) "MemberType")))

(defun get-member-info (clr-name type arg-types)
  (let ((type-object (to-clr-type-object type))
        member-info)
    ;; A note regarding the GetProperty/GetMethod calls below: It's
    ;; not obvious from the MSDN docs, but the last parameter, a
    ;; ParameterInfo array, is marked with the "ParameterArray"
    ;; attribute, meaning that it actually accepts trailing varying
    ;; args and can thus be omitted.
    (unless arg-types
      (setf member-info (invoke-instance type-object "GetField" clr-name)))
    (unless member-info
      (setf member-info (invoke-instance type-object
                                         "GetProperty"
                                         clr-name
                                         (binding-flags "Instance"
                                                        "Static"
                                                        "Public"
                                                        "FlattenHierarchy")
                                         *lisp-binder*
                                         nil
                                         (arg-types-to-types-array arg-types))))
    (unless member-info
      (setf member-info (invoke-instance type-object
                                         "GetMethod"
                                         clr-name
                                         (binding-flags "Instance"
                                                        "Static"
                                                        "Public"
                                                        "FlattenHierarchy")
                                         *lisp-binder*
                                         (arg-types-to-types-array arg-types))))
    (unless member-info
      (error "No member named ~S found on type ~S taking these arguments:~%~
               "
             clr-name (invoke-instance type-object "FullName")))
    member-info))

(defvar *clr-call-symbols* nil)

(defmacro define-clr-call (type name &rest arg-defs)
  "Define a Lisp function that invokes a specific CLR
member. Also defines a SETF function when appropriate. NAME can
be a string with the CLR member name, or a symbol denoting the
Lisp function. In either case, the other name is generated via a
simple algorithm that converts mixed-case to hyphenated or vice
versa. Altnernately, NAME may be a list of CLR name string and
Lisp symbol. TYPE must be a CLR type designator of the type which
defines or inheritsthe member. ARG-DEFS are two-element lists
denoting the arguments, first the name symbol and then a CLR type
designator. For instance methods, the target object itself should
not be included in the argument list. None of the arguments are
evaluated."  
  (let* ((clr-name (etypecase name
                     (list (first name))
                     (string name)
                     (symbol (lisp-to-clr-name (symbol-name name)))))
         (lisp-name (etypecase name
                      (list (second name))
                      (string (read (make-string-input-stream
                                     (clr-to-lisp-name name))))
                      (symbol name)))
         (member-info (get-member-info clr-name
                                       type
                                       (map 'list #'second arg-defs)))
         (type-object (to-clr-type-object type))
         (member-kind (invoke-instance member-info "MemberType")))
    ;; A note regarding the GetProperty/GetMethod calls below: It's
    ;; not obvious from the MSDN docs, but the last parameter, a
    ;; ParameterInfo array, is marked with the "ParameterArray"
    ;; attribute, meaning that it actually accepts trailing varying
    ;; args and can thus be omitted.
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',lisp-name 'member-info)
             (get-member-info ,clr-name ',type ',(map 'list #'second arg-defs)))
       (setf (get ',lisp-name 'clr-call-info)
             '(,clr-name ',type ',(map 'list #'second arg-defs)))
       (push ',lisp-name *clr-call-symbols*)
       ,@(cond
          ((eql member-kind (enum-value "System.Reflection.MemberTypes"
                                        "Field"))
           (make-field-definition-forms lisp-name member-info))
          ((eql member-kind (enum-value "System.Reflection.MemberTypes"
                                        "Property"))
           (make-property-definition-forms lisp-name member-info arg-defs))
          ((eql member-kind (enum-value "System.Reflection.MemberTypes"
                                        "Method"))
           (list (make-method-definition-form lisp-name member-info arg-defs)))
          (t
           (error "No member named ~S found on type ~S."
                  clr-name type)))
       ',lisp-name)))

(defun init-clr-define ()
  (map nil
       #'(lambda (sym)
           (setf (get sym 'member-info)
                 (apply #'get-member-info (get sym 'clr-call-info))))
       *clr-call-symbols*))

(defun shutdown-clr-define ()
  (map nil
       #'(lambda (sym)
           (setf (get sym 'member-info) nil))
       *clr-call-symbols*))