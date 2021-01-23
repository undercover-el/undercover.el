;;; first-example-library-test.el --- undercover.el: Unit-test suite -*- lexical-binding: t; -*-

;; Copyright (c) 2014 Sviridov Alexander

;; Author: Sviridov Alexander <sviridov.vmi@gmail.com>

;;; Commentary:

;; The unit test suite of undercover.el.

;;; Code:

(defconst first-example-library-filename
  "test/first-example-library/first-example-library.el")

(defconst first-example-library-report-file
  "/tmp/first-example-library-report.json")

(defmacro with-env-variable (name value &rest body)
  "Set environment variable NAME to VALUE and evaluate BODY."
  (declare (indent 2))
 `(let ((---old-env-var--- (getenv ,name)))
    (setenv ,name ,value)
    (setq undercover--env nil) ; Clear cached environment
    (unwind-protect (progn ,@body)
      (setenv ,name ---old-env-var---))))

(with-env-variable "TRAVIS" "true"
  (let ((undercover-force-coverage nil))
    (undercover "test/first-example-library/*.el"
      (:report-file first-example-library-report-file)
      (:send-report nil))
    (ignore-errors (delete-file first-example-library-report-file))
    (add-to-list 'load-path (file-truename "test/first-example-library"))
    (require 'first-example-library)))

(ert-deftest test-001/edebug-handlers-are-setted ()
  (if (boundp 'edebug-behavior-alist)
      (should (assq 'undercover edebug-behavior-alist))
    (should (eq 'undercover--stop-point-before (symbol-function 'edebug-before)))
    (should (eq 'undercover--stop-point-after (symbol-function 'edebug-after)))))

(ert-deftest test-002/result-is-correct ()
  (should (= 1.0 (distance '(0 0) '(1 0))))
  (should (= 5.0 (distance '(3 3) '(6 7))))

  (should (= 0 (fib 0)))
  (should (= 1 (fib 1)))
  (should (= 8 (fib 6))))

(ert-deftest test-003/functions-are-covered ()
  (should (get 'distance 'edebug))
  (should (get 'fib 'edebug)))

(ert-deftest test-004/check-distance-stop-points-number-of-covers ()
  (dolist (stop-point-covers (undercover--stop-points-covers 'distance))
    (should (= stop-point-covers 2))))

(ert-deftest test-005/check-coverage-statistics ()
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

(ert-deftest test-006/check-environment-variables ()
  (with-env-variable "TRAVIS" "true"
    (should (eq 'coveralls (undercover--detect-report-format)))))

(defun coveralls--check-lines-statistics (multiplier example-library-statistics)
  ;; distance statistics
  (dolist (line '(14 15 16 17 18 19))
    (should (= (* multiplier 2) (nth line example-library-statistics))))

  (should-not (nth 13 example-library-statistics))
  (should-not (nth 20 example-library-statistics))

  ;; fib statistics
  (should (= (* multiplier 27) (nth 23 example-library-statistics)))
  (should (= (* multiplier 27) (nth 24 example-library-statistics)))
  (should (= (* multiplier 21) (nth 25 example-library-statistics)))
  (should (= (* multiplier 12) (nth 26 example-library-statistics))))

(ert-deftest test-007/check-coveralls-report ()
  (with-env-variable "TRAVIS" "true"
    (ad-deactivate 'undercover-safe-report)
    (undercover-safe-report)
    (ad-activate 'undercover-safe-report))

  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (report (json-read-file first-example-library-report-file)))

    (should (string-equal "travis-ci" (gethash "service_name" report)))

    (let ((file-report (car (gethash "source_files" report))))
      (should (string-equal "test/first-example-library/first-example-library.el"
                            (gethash "name" file-report)))

      (should (string-equal (save-excursion
                              (find-file (file-truename "test/first-example-library/first-example-library.el"))
                              (buffer-substring-no-properties (point-min) (point-max)))

                            (gethash "source" file-report)))

      (coveralls--check-lines-statistics 1 (gethash "coverage" file-report))
      (undercover-coveralls--merge-reports report)
      (coveralls--check-lines-statistics 2 (gethash "coverage" file-report)))))

(ert-deftest test-008/should-error ()
  (with-env-variable "TRAVIS" nil
    (should-error (undercover-report))
    (should-error (undercover--create-coveralls-report))))

(ert-deftest test-009/check-simplecov-report ()
  ;; Don't attempt to merge with report in another format
  (when (file-readable-p first-example-library-report-file)
    (delete-file first-example-library-report-file))
  (undercover-report 'simplecov)

  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (reportset (json-read-file first-example-library-report-file)))

    (let* ((report (gethash "undercover.el" reportset))
           (coverage (gethash "coverage" report))
           (file-key (file-truename "test/first-example-library/first-example-library.el")))

      (coveralls--check-lines-statistics 1 (gethash file-key coverage))
      (undercover-simplecov--merge-reports reportset)
      (coveralls--check-lines-statistics 2 (gethash file-key coverage)))))

(ert-deftest test-010/check-text-report ()
  (let* ((undercover--files-coverage-statistics (make-hash-table :test 'equal))
         (undercover--files (list (file-truename "test/first-example-library/first-example-library.el")))
         (report (undercover-text--create-report)))
    (should (string-equal report "== Code coverage text report ==
first-example-library : Percent 100% [Relevant: 10 Covered: 10 Missed: 0]
"))))

(ert-deftest test-011/check-text-report-file ()
  (let* ((undercover--files-coverage-statistics (make-hash-table :test 'equal))
         (undercover--files (list (file-truename "test/first-example-library/first-example-library.el")))
         (undercover--report-file-path "/tmp/undercover-text-report.txt"))
    (undercover-report 'text)
    (let ((report (with-temp-buffer
                    (insert-file-contents undercover--report-file-path)
                    (buffer-substring-no-properties (point-min) (point-max)))))
      (should (string-equal report "== Code coverage text report ==
first-example-library : Percent 100% [Relevant: 10 Covered: 10 Missed: 0]
")))))

;;; first-example-library-test.el ends here
