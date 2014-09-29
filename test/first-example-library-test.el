;;; first-example-library-test.el --- undercover.el: Unit-test suite -*- lexical-binding: t; -*-

;; Copyright (c) 2014 Sviridov Alexander

;; Author: Sviridov Alexander <sviridov.vmi@gmail.com>

;;; Commentary:

;; The unit test suite of undercover.el.

;;; Code:

(defconst first-example-library-filename
  "test/first-example-library/first-example-library.el")

(let ((undercover-force-coverage t))
  (undercover first-example-library-filename))

(defmacro with-env-variable (name value &rest body)
  "Set environment variable NAME to VALUE and evaluate BODY."
 `(let ((---old-env-var--- (getenv ,name)))
    (setenv ,name ,value)
    (unwind-protect (progn ,@body)
      (setenv ,name ---old-env-var---))))

(defun assoc-cdr (key alist) (cdr (assoc key alist)))

(ert-deftest test-1/edebug-handlers-are-setted ()
  (should (eq 'undercover--stop-point-before (symbol-function 'edebug-before)))
  (should (eq 'undercover--stop-point-after (symbol-function 'edebug-after))))

(ert-deftest test-2/result-is-correct ()
  (should (= 1.0 (distance '(0 0) '(1 0))))
  (should (= 5.0 (distance '(3 3) '(6 7))))

  (should (= 0 (fib 0)))
  (should (= 1 (fib 1)))
  (should (= 8 (fib 6))))

(ert-deftest test-3/functions-are-covered ()
  (should (get 'distance 'edebug))
  (should (get 'fib 'edebug)))

(ert-deftest test-4/check-distance-stop-points-number-of-covers ()
  (dolist (stop-point-covers (undercover--stop-points-covers 'distance))
    (should (= stop-point-covers 2))))

(ert-deftest test-5/check-coverage-statistics ()
  (undercover--collect-files-coverage undercover--files)
  (let ((example-library-statistics (gethash (file-truename first-example-library-filename)
                                             undercover--files-coverage-statistics)))
    ;; distance statistics
    (dolist (line '(15 16 17 18 19 20))
      (should (= 2 (gethash line example-library-statistics))))

    (should-not (gethash 14 example-library-statistics))
    (should-not (gethash 21 example-library-statistics))

    ;; fib statistics
    (should (= 27 (gethash 24 example-library-statistics)))
    (should (= 27 (gethash 25 example-library-statistics)))
    (should (= 21 (gethash 26 example-library-statistics)))
    (should (= 12 (gethash 27 example-library-statistics)))))

(ert-deftest test-6/check-environment-variables ()
  (with-env-variable "TRAVIS" "true"
    (should (eq 'coveralls (undercover--determine-report-type)))))

(ert-deftest test-7/check-coveralls-report ()
  (with-env-variable "TRAVIS" "true"
    (with-mock
      (stub shell-command)
      (undercover-report)))

  (let ((report (json-read-file "/tmp/json_file")))
    (should (string-equal "travis-ci" (assoc-cdr 'service_name report)))
    (let ((file-report (aref (assoc-cdr 'source_files report) 0)))
      (should (string-equal "test/first-example-library/first-example-library.el"
                            (assoc-cdr 'name file-report)))

      (should (string-equal (save-excursion
                              (find-file (file-truename "test/first-example-library/first-example-library.el"))
                              (buffer-substring-no-properties (point-min) (point-max)))

                            (assoc-cdr 'source file-report)))

      (let ((example-library-statistics (assoc-cdr 'coverage file-report)))
        ;; distance statistics
        (dolist (line '(14 15 16 17 18 19))
          (should (= 2 (aref example-library-statistics line))))

        (should-not (aref example-library-statistics 13))
        (should-not (aref example-library-statistics 20))

        ;; fib statistics
        (should (= 27 (aref example-library-statistics 23)))
        (should (= 27 (aref example-library-statistics 24)))
        (should (= 21 (aref example-library-statistics 25)))
        (should (= 12 (aref example-library-statistics 26)))))))

;;; first-example-library-test.el ends here
