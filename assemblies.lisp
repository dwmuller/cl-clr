(in-package :cl-clr)

(enable-clr-syntax)

(defmacro do-assemblies (var &body body)
  "Execute body once for each assembly known to CL-CLR in the
current application domain, with VAR bound to the corresponding
System.Reflection.Assembly object. Returns nothing. (Currently,
this iterates through all assemblies in the application domain.)"
  `(do-rdnzl-array
       (,var (?.GetAssemblies (?.CurrentDomain '?System.AppDomain)))
     ,@body))
       
(disable-clr-syntax)