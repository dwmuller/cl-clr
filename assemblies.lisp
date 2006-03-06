(in-package :cl-clr)

(defun %load-assembly (&rest args)
  (force-type (apply #'invoke
                     (property "System.AppDomain" "CurrentDomain")
                     "Load"
                     args)))
(defmacro load-assembly (&rest args)
  "Loads an assembly into the current application domain using
System.AppDomain.Load(). Arranges for the assembly to be included
in searches for types by CL-CLR. Returns the assembly."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%load-assembly ,@args)))

(defmacro do-assemblies (var &body body)
  "Execute body once for each assembly known to CL-CLR in the
current application domain, with VAR bound to the corresponding
System.Reflection.Assembly object. Returns nothing. (Currently,
this iterates through all assemblies in the application domain.)"
  `(do-rdnzl-array
       (,var (invoke (invoke "System.AppDomain" "CurrentDomain")
                     "GetAssemblies "))
     ,@body))

