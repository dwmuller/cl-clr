;;;
;;; This code comes directly from Practical Common Lisp, Chapter 9.
;;; Thanks go to the author, Peter Seibel.
;;;
;;; The original was under 30 lines of code!
;;;
;;; It has been modified to allow tests to run even when error
;;; conditions are raised.
;;;
(in-package :pcl-unit-test-framework)

(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (let ((result (gensym)))
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defun print-result (result form condition)
  (format t "~:[FAIL~;pass~] ... ~A: ~S~@[~%    ~
                  Error condition of type ~a signalled.~]~@[~%~
                  ~<    ~@;~A~>~]~%"
          result
          *test-name*
          form
          (when condition (type-of condition))
          condition))

(defmacro report-result (form)
  "Report the results of a single test case. Called by 'check'."
  (let ((result (gensym))
        (condition (gensym))
        (single-result (gensym)))
    `(multiple-value-bind (,result ,condition)
         (ignore-errors
           ;; Just in case form returns multiple values, make sure
           ;; that we return only one:
           (multiple-value-bind (,single-result) ,form ,single-result))
       (print-result ,result ',form ,condition)
       ,result)))
