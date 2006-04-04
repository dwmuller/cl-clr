;;; $Id: packages.lisp 78 2006-04-02 15:48:54Z  $
;;;
;;; Copyright (c) 2006, Dan Muller. See accompanying LICENSE.txt file.
;;;

(in-package :cl-clr)

(defun enum-to-integer (enum)
  ;; This is surprisingly tortuous... perhaps there's a better way?
  (invoke-static *system-convert-type* "ChangeType"
                 enum
                 (invoke-static (get-system-type "System.Enum")
                                "GetUnderlyingType"
                                (symbol-to-clr-type-object
                                 (clr-type-of enum)))))
(defun integer-to-enum (int type)
  (invoke-static (get-system-type "System.Enum")
                 "ToObject"
                 (type-arg-to-type-object type)
                 int))

(defun or-enums (&rest args)
  (let ((type (clr-type-of (first args))))
    (integer-to-enum
     (reduce #'(lambda (value enum)
                 (logior value (enum-to-integer enum)))
             args
             :initial-value 0) type)))



