;;; $Id: packages.lisp 78 2006-04-02 15:48:54Z  $
;;;
;;; Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.
;;;

(in-package :cl-clr)

(defun enum-value (type name)
  "Returns an enumeration value.
TYPE is a CLR type designator (a symbol, string, or System.Type
object) for a type derived from System.Enum. NAME can be a symbol
or a string denoting a static field of that Enum type, or an
integer."
  (%handle-to-value
   (%signal-if-exception (%enum-value (type-arg-to-type-object type)
                                      (etypecase name
                                        (symbol (get name 'clr-member))
                                        (string name))))))

(defun or-enum-values (type &rest args)
  "Fetch and combine several enumeration values using
LOGIOR. TYPE is a CLR type designator (a symbol, string, or
System.Type object) for a type derived from System.Enum. Each
remaining argument can be a symbol or a string denoting a static field of
that Enum type, or an integer."
  (let ((type (type-arg-to-type-object type)))
    (reduce #'(lambda (value arg)
                (logior value (if (typep arg 'integer)
                                  arg
                                  (enum-value type arg))))
            args :initial-value 0)))




