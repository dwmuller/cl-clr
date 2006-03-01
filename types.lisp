(in-package :cl-clr)

(defun find-assembly-qualifier (type-name)
  "TYPE-NAME is assumed to be a type name string. If it does not
contain any unescaped commas, then the name is not
assembly-qualified and NIL is returned. Otherwise, the index of
the first unescaped comma is returned."
  (loop
     for pos = (position #\, type-name)
     then (position #\, type-name :start (1+ pos))
     initially (when (and pos (zerop pos))
                 (error "Empty type name: ~S" type-name))
     if (not pos) return nil
     if (and pos (not (eql (aref type-name (1- pos)) #\\)))
     return pos))

(defun elide-assembly (type-name)
  (let ((qpos (find-assembly-qualifier type-name)))
    (if qpos
        (subseq type-name 0 qpos)
        type-name)))
        
(defun is-namespace-qualified-type-name (type-name &optional end)
  "TYPE-NAME is assumed to be a type name string. If END is non-nil,
it is assumed to be the index of the comma denoting an assembly
qualifier. If there is at least one unescaped period before the
end of the string or END, then IS-NAMESPACE-QUALIFIED-TYPE-NAME
returns T, otherwise NIL."
  (loop
     for pos = (position #\. type-name)
     then (position #\. type-name :start (1+ pos))
     and endpos = (or end
                      (find-assembly-qualifier type-name)
                      (length type-name))
     if (or (not pos) (> pos endpos)) return nil
     if (and pos (or (zerop pos) (not (eql (aref type-name (1- pos)) #\\))))
     return t))

(defun find-type-from-namespace-qualified-name (type-name)
  "Returns a System.Type object for the type in the given
namespace with the given simple-name. It searches all assemblies
loaded in the current application domain. Signals an error if the
type is not found, or if it is ambiguous.

All CLR types referenced in this function must be preloaded by
INIT-TYPE-OBJECTS."
  (let ((result nil)
        (app-domain (property "System.AppDomain" "CurrentDomain")))
    (do-rdnzl-array
        (assembly (invoke app-domain "GetAssemblies"))
      (let ((type (invoke assembly "GetType" type-name)))
        (when (and type (property type "IsPublic"))
          (typecase result
            (null (setf result type))
            (list (push type result))
            (t (setf result (list type result)))))))
    (typecase result
      (null
       (error "Could not find an accessible public type named ~S."
              type-name))
      (list
       (error "Type ~S is multiply defined in these assemblies:~%~{  ~A~%~}"
              type-name
              (map 'list
                   (lambda (type)
                     (property (property type "Assembly") "FullName"))
                   result)))
      (t result))))

(defun find-type-from-simple-name (simple-name namespaces)
  "Returns a System.Type object for the type with the given
simple-name. It searches all assemblies loaded in the current
application domain, and all given namespaces. Signals an error if
the type is not found, or if it is ambiguous."
  (let ((result nil)
        (app-domain (property "System.AppDomain" "CurrentDomain")))
    (loop
       for namespace in (cons "" namespaces)
       for qualified-name = (concatenate 'string namespace "." simple-name)
       do (do-rdnzl-array
              (assembly (invoke app-domain "GetAssemblies"))
            (let ((type (invoke assembly "GetType" qualified-name)))
              (when (and type
                         (property type "IsPublic"))
                (typecase result
                  (null (setf result type))
                  (t (push type result))
                  (symbol (setf result (list type result))))))))
    (typecase result
      (null
       (error "Could not find an accessible public type named ~S.~%Searched these namespaces:~%~{  ~A~%~}"
              simple-name
              namespaces))
      (list
       (error "Type name ~S is ambiguous. Could refer to any of:~%~{  ~A~%~}"
              simple-name
              (map 'list
                   (lambda (type) (property type "FullName"))
                   result)))
      (t result))))

(defun find-type-from-name (type-name namespaces)
  "Returns a System.Type object for the given type-name. If the
type-name is fully qualified, the type object is retrieved using
System.Type.GetType(). If the name is qualified by namespace but
not by assembly, the type is returned if it is unique in the
current application domain. If the type-name is not qualified at
all, then the type is returned if it is defined exactly once in
the global namespace or the currently used namespaces.  An error
is signaled if the type-name is ambiguous or undefined."
  (let ((assembly-part (find-assembly-qualifier type-name)))
    (cond
      (assembly-part
       (invoke "System.Type" "GetType" type-name))
      ((is-namespace-qualified-type-name type-name assembly-part)
       (find-type-from-namespace-qualified-name type-name))
      (t
       (find-type-from-simple-name type-name namespaces)))))

