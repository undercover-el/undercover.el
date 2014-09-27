;;; test-helper.el --- undercover.el: Unit-test suite -*- lexical-binding: t; -*-

;; Copyright (c) 2014 Sviridov Alexander

;; Author: Sviridov Alexander <sviridov.vmi@gmail.com>

;;; Commentary:

;; The unit test suite of undercover.el.

;;; Code:

(defvar undercover-test-number-of-calls (make-hash-table)
  "Table that store number of example-library functions calls.")

(defmacro undercover-test-call (name &rest args)
  "Register of call of NAME with ARGS."
 `(progn
    (incf (gethash ',name undercover-test-number-of-calls 0))
    (,name ,@args)))

(ert-deftest result-is-correct ()
  (should (= 1.0 (undercover-test-call distance '(0 0) '(1 0))))
  (should (= 5.0 (undercover-test-call distance '(3 3) '(6 7)))))

(ert-deftest functions-are-covered ()
  (should (get 'distance 'edebug)))

(ert-deftest edebug-handlers-are-setted ()
  (should (eq 'undercover--stop-point-before (symbol-function 'edebug-before)))
  (should (eq 'undercover--stop-point-after (symbol-function 'edebug-after))))

(ert-deftest check-distance-stop-points-number-of-covers ()
  (undercover-test-call distance '(0 0) '(1 0))
  (dolist (stop-point-covers (undercover--stop-points-covers 'distance))
    (should (= stop-point-covers (gethash 'distance undercover-test-number-of-calls)))))

;;; undercover-test.el ends here
