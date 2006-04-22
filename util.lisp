;;; $Id$
;;;
;;; Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.
;;;

(in-package :cl-clr)

(enable-clr-syntax)
(use-namespaces "System"
                "System.Reflection"
                "SpookyDistance.CommonLispReflection")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Array creation

(defun list-to-clr-array (list &key (element-type *system-object-type*))
  "Creates and returns a CLR array of rank 1 with element type
ELEMENT-TYPE (a CLR-OBJECT, type name string, or symbol
representing a CLR type) with the elements from the Lisp list
LIST. BASE-TYPE defaults to System.Object."
  (check-type list sequence)
  (setf element-type (type-arg-to-type-object element-type))
  (let ((array (?Array.CreateInstance (type-arg-to-type-object element-type)
                                      (length list))))
    (loop
       for item in list
       for i from 0
       do
         (setf (aref* array i) item))
    array))

(defun as-var-args (seq &key (element-type *system-object-type*))
  "Takes a list or a System.Array object of rank 1 and wraps it
in an object that will match a method's final parameter if it has
the ParamArray attribute. This is used in cases where you have a
variable list of arguments that are more conveniently provided as
a prebuilt sequence. In CL-CLR, this is the only object that will
directly match such a parameter, unlike C# which will implicitly
match an array argument. The ELEMENT-TYPE keyword is only used if
SEQ is a list, and defaults to System.Object. "
  (%handle-to-value (%wrap-varargs-array
                     (etypecase seq
                       (list (list-to-clr-array seq :element-type element-type))
                       (clr-object seq)))))

(defun print-members (type &optional member-name)
  (let ((members-info
         (if member-name
             (?GetMember (type-arg-to-type-object type)
                         member-name
                         (binding-flags "Static"
                                        "Public"
                                        "FlattenHierarchy"))
             (?GetMembers (type-arg-to-type-object type)))))
    (do-clr-array (member-info members-info)
      (format t "~&~S: ~A ~A "
              (?Name member-info)
              (?Name (?DeclaringType member-info))
              (?Name (?ReflectedType member-info))))))

(defun print-types (prefix &optional full)
  (do-clr-array (assembly
                   (?GetAssemblies (?System.AppDomain.CurrentDomain)))
      (do-clr-array (type-object (?GetExportedTypes assembly))
        (when (zerop (or (search prefix (?FullName type-object)) 1))
              (format t "~&~A"
                      (if full
                          (?AssemblyQualifiedName type-object)
                          (?FullName type-object)))))))

(bind-clr-symbols)