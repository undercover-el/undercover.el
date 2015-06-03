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
 `(let ((---old-env-var--- (getenv ,name)))
    (setenv ,name ,value)
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
    (ad-deactivate 'undercover-safe-report)
    (undercover-safe-report)
    (ad-activate 'undercover-safe-report))

  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (report (json-read-file first-example-library-report-file)))

    (cl-flet ((check-lines-statistics (multiplier example-library-statistics)
                ;; distance statistics
                (dolist (line '(14 15 16 17 18 19))
                  (should (= (* multiplier 2) (nth line example-library-statistics))))

                (should-not (nth 13 example-library-statistics))
                (should-not (nth 20 example-library-statistics))

                ;; fib statistics
                (should (= (* multiplier 27) (nth 23 example-library-statistics)))
                (should (= (* multiplier 27) (nth 24 example-library-statistics)))
                (should (= (* multiplier 21) (nth 25 example-library-statistics)))
                (should (= (* multiplier 12) (nth 26 example-library-statistics)))))

      (should (string-equal "travis-ci" (gethash "service_name" report)))

      (let ((file-report (car (gethash "source_files" report))))
        (should (string-equal "test/first-example-library/first-example-library.el"
                              (gethash "name" file-report)))

        (should (string-equal (save-excursion
                                (find-file (file-truename "test/first-example-library/first-example-library.el"))
                                (buffer-substring-no-properties (point-min) (point-max)))

                              (gethash "source" file-report)))

        (check-lines-statistics 1 (gethash "coverage" file-report))
        (undercover--merge-coveralls-reports report)
        (check-lines-statistics 2 (gethash "coverage" file-report))))))

(ert-deftest test-8/should-error ()
  (with-env-variable "TRAVIS" nil
    (should-error (undercover-report))
    (should-error (undercover--create-coveralls-report))))

;;; first-example-library-test.el ends here
