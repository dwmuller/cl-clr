;;; $Id$
;;;
;;; Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.
;;;
;;; Functions having to do with the manipulation of type names and
;;; looking up CLR type objects.
;;;
;;; This file is loaded early, because type lookup is used by
;;; much of the rest of CL-CLR. Thus it avoids using higher-level
;;; functionality, using e.g. the INVOKE-* functions which don't
;;; understand symbols-as-types, and avoiding the use of DO-ASSEMBLIES.
;;;

(in-package :cl-clr)

(defun unescaped-char-position (char string &key (start 0) (end nil))
  (loop
     with escaped = nil
     for pos from start below (or end (length string))
     for candidate = (aref string pos)
     do
       (cond
         (escaped              (setf escaped nil))
         ((eql candidate char) (return-from unescaped-char-position pos))
         ((eql #\\ candidate)  (setf escaped t)))
     until (and (not escaped)
                (eql char candidate)))
  nil)

(defun find-assembly-separator (type-name)
  "TYPE-NAME is assumed to be a type name string. If it does not
contain any unescaped commas, then the name is not
assembly-qualified and NIL is returned. Otherwise, the index of
the first unescaped comma is returned."
  (unescaped-char-position #\, type-name))

(defun find-type-namespace-separator (type-name &key end)
  (let (period)
    (loop
       for pos = (unescaped-char-position #\. type-name :end end)
       then (unescaped-char-position #\. type-name :start (1+ pos) :end end)
       while pos
       do
         (setf period pos))
    period))
  
(defun split-type-name (type-name)
  "Splits the CLR type name string TYPE-NAME into three parts,
which are returned as separate strings: The namespace name, the
type name, and the assembly name. If a part is missing, NIL is
returned for that value."
  (let* ((comma  (find-assembly-separator type-name))
         (period (find-type-namespace-separator type-name :end comma)))
    (cond ((and period comma)
           (values (subseq type-name 0 period)
                   (subseq type-name (1+ period) comma)
                   (subseq type-name (1+ comma))))
          (period
           (values (subseq type-name 0 period)
                   (subseq type-name (1+ period))
                   NIL))
          (comma
           (values nil
                   (subseq type-name 0 comma)
                   (subseq type-name (1+ comma))))
          (t (values nil type-name nil)))))

(defun elide-assembly (type-name)
  "TYPE-NAME is a CLR type name string. Remove the assembly
qualifier, if any, and return the result."
  (let ((qpos (find-assembly-separator type-name)))
    (if qpos
        (subseq type-name 0 qpos)
        type-name)))
        
(defun is-namespace-qualified-type-name (type-name &optional end)
  "TYPE-NAME is assumed to be a type name string. If END is non-nil,
it is assumed to be the index of the comma denoting an assembly
qualifier. If there is at least one unescaped period before the
end of the string or END, then IS-NAMESPACE-QUALIFIED-TYPE-NAME
returns T, otherwise NIL."
  (unless end
    (setf end (or (find-assembly-separator type-name)
                  (length type-name))))
  (loop
     for pos = (position #\. type-name)
     then (position #\. type-name :start (1+ pos))
     if (or (not pos) (> pos end)) return nil
     if (and pos (or (zerop pos) (not (eql (aref type-name (1- pos)) #\\))))
     return t))

(define-condition clr-type-undefined (error)
  ((type-name :type string
              :accessor type-name-of
              :initarg :type-name)
   (assemblies :type list
               :accessor assemblies-searched
               :initarg :assemblies)))

(define-condition clr-type-multiply-defined (error)
  ((type-name :type list
              :accessor type-name-of
              :initarg :type-name)
   (assemblies :type list
               :accessor defining-assemblies
               :initarg defining-assemblies)))

(define-condition clr-type-not-found (error)
  ((type-name :type string
              :accessor type-name-of
              :initarg :type-name)
   (namespaces :type list
               :accessor namespaces-searched
               :initarg :namespaces)))

(define-condition clr-type-ambiguous (error)
  ((type-name :type string
              :accessor type-name-of
              :initarg :type-name)
   (candidates :type list
               :accessor candidates-of
               :initarg :candidates)))

(defmethod print-object ((x clr-type-undefined) stream)
  (format stream
          "Could not find an accessible public type named ~S.~%~
           Searched these assemblies:~%~{  ~A~%~}"
          (type-name-of x)
          (map 'list #'(lambda (a)
                         (invoke-instance a "FullName"))
               (assemblies-searched x))))

(defmethod print-object ((x clr-type-multiply-defined) stream)
  (format stream
          "Type ~S is multiply defined in these assemblies:~%~{  ~A~%~}"
          (type-name-of x)
          (map 'list
               #'(lambda (type)
                   (invoke-instance (invoke-instance type "Assembly")
                                    "FullName"))
               (defining-assemblies x))))

(defmethod print-object ((x clr-type-not-found) stream)
  (format stream
          "Could not find an accessible public type named ~S.~%~
           Searched these namespaces:~%~{  ~A~%~}"
          (type-name-of x)
          (namespaces-searched x)))

(defmethod print-object ((x clr-type-ambiguous) stream)
  (format stream
          "Type name ~S is ambiguous. Could refer to any of:~%~{  ~A~%~}"
          (type-name-of x)
          (candidates-of x)))
  
(defun find-type-from-full-name (type-name)
  "Returns a System.Type object for the type in the given
namespace with the given simple-name. It searches all assemblies
loaded in the current application domain. Signals an error if the
type is not found, or if it is ambiguous.

All CLR types referenced in this function must be preloaded by
INIT-TYPE-OBJECTS."
  (let ((result nil))
    (do-clr-array
        (assembly (invoke-instance *default-app-domain* "GetAssemblies"))
      (let ((type (invoke-instance assembly "GetType" type-name)))
        (when (and type (or (invoke-instance type "IsPublic")
                            (invoke-instance type "IsNestedPublic")))
          (typecase result
            (null (setf result type))
            (list (push type result))
            (t (setf result (list type result)))))))
    (typecase result
      (null
       (error (make-condition 'clr-type-undefined
                              :type-name type-name
                              :assemblies
                              (clr-array-to-list
                               (invoke-instance *default-app-domain*
                                                "GetAssemblies")))))
      (list
       (error (make-condition 'clr-type-ambiguous
                              :type-name type-name
                              :assemblies result))) 
      (t result))))

(defun find-type-from-simple-name (simple-name namespaces)
  "Returns a System.Type object for the type with the given
simple-name. It searches all assemblies loaded in the current
application domain, and all given namespaces. Signals an error if
the type is not found, or if it is ambiguous."
  (let ((result nil))
    (loop
       for namespace in (cons "" namespaces)
       for qualified-name = (concatenate 'string namespace "." simple-name)
       do (do-clr-array
              (assembly (invoke-instance *default-app-domain* "GetAssemblies"))
            (let ((type (invoke-instance assembly "GetType" qualified-name)))
              (when (and type
                         (or (invoke-instance type "IsPublic")
                             (invoke-instance type "IsNestedPublic")))
                (typecase result
                  (null (setf result type))
                  (list (push type result))
                  (t (setf result (list type result))))))))
    (typecase result
      (null
       (error (make-condition 'clr-type-not-found
                              :type-name simple-name
                              :namespaces namespaces)))
      (list
       (error (make-condition 'clr-type-ambiguous
                              :type-name simple-name
                              :candidates
                              (map 'list
                                   (lambda (type)
                                     (invoke-instance type "FullName"))
                                   result))))
      (t result))))

(defun find-type-from-name (type-name namespaces)
  "Returns a System.Type object for the given type-name. If the
type-name is fully qualified, the type object is retrieved using
System.Type.GetType(). If the name is qualified by namespace but
not by assembly, the type is returned if it is unique in the
current application domain. If the type-name is not qualified at
all, then the type is returned if it is defined exactly once in
the global namespace or the specified namespaces.  An error
is signaled if the type-name is ambiguous or undefined."
  (let ((assembly-part (find-assembly-separator type-name)))
    (cond
      (assembly-part
       (invoke-static *system-type-type* "GetType" type-name))
      ((is-namespace-qualified-type-name type-name)
       (find-type-from-full-name type-name))
      (t
       (find-type-from-simple-name type-name namespaces)))))

