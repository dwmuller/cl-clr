;;; $Id$
;;;
;;; Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.
;;;
(in-package :cl-clr)

(defmacro load-assembly (&rest args)
  "Loads assembly into the current application domain using
System.AppDomain.Load(). The arguments are as for that method."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (apply #'invoke-member *default-app-domain* "Load" ',args)))

(defmacro do-assemblies (var &body body)
  "Execute body once for each assembly known to CL-CLR in the
current application domain, with VAR bound to the corresponding
System.Reflection.Assembly object. Returns nothing. (Currently,
this iterates through all assemblies in the application domain.)"
  `(do-clr-array
       (,var (invoke-member (invoke-static (get-system-type "System.AppDomain")
                                           "CurrentDomain")
                            "GetAssemblies "))
     ,@body))

