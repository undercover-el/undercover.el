;;; first-example-library-test.el --- undercover.el: Unit-test suite -*- lexical-binding: t; -*-

;; Copyright (c) 2014 Sviridov Alexander

;; Author: Sviridov Alexander <sviridov.vmi@gmail.com>

;;; Commentary:

;; The unit test suite of undercover.el.

;;; Code:

(eval-and-compile
  (defun undercover-root ()
    (if load-file-name
        ;; Cask
        (file-name-directory
         (directory-file-name
          (file-name-directory
           load-file-name)))
      ;; Flycheck
      (file-name-directory
       (directory-file-name
        default-directory)))))

(eval-when-compile
  (add-to-list 'load-path
               (undercover-root)))

(require 'advice)
(require 'undercover)

(defconst first-example-library-filename
  "test/first-example-library/first-example-library.el")

(defconst first-example-library-report-file
  "/tmp/first-example-library-report.json")

(defun undercover--clean-environment ()
  (--filter (string-prefix-p "HOME=" it)
            process-environment))

(defmacro with-env-variable (name value &rest body)
  "Set environment variable NAME to VALUE and evaluate BODY."
  (declare (indent 2))
  `(let ((process-environment (undercover--clean-environment)))
     (setenv ,name ,value)
     (setq undercover--env nil) ; Clear cached environment
     (progn ,@body)))

(with-env-variable "TRAVIS" "true"
  (let ((undercover-force-coverage nil))
    (undercover "test/first-example-library/*.el"
                (:report-file first-example-library-report-file)
                (:send-report nil)
                (:verbosity 10))
    (ignore-errors (delete-file first-example-library-report-file))))

(eval-and-compile
  (add-to-list 'load-path
               (expand-file-name "test/first-example-library" (undercover-root))))
(require 'first-example-library)

(ert-deftest test/1-setup/edebug-handlers-are-setted ()
  (if (boundp 'edebug-behavior-alist)
      (should (assq 'undercover edebug-behavior-alist))
    (should (eq 'undercover--stop-point-before (symbol-function 'edebug-before)))
    (should (eq 'undercover--stop-point-after (symbol-function 'edebug-after)))))

(ert-deftest test/1-setup/wildcards ()
  (should (equal (undercover--wildcards-to-files nil)
                 nil))
  (should (equal (undercover--wildcards-to-files '("*.el"))
                 '("undercover.el")))
  (should (equal (undercover--wildcards-to-files '("*.el"
                                                   (:exclude "under*.el")))
                 nil))
  (should (equal (undercover--wildcards-to-files '("*.el"
                                                   (:exclude "under*.el")
                                                   "underco*.el"))
                 '("undercover.el")))
  (should (equal (undercover--wildcards-to-files '((:files "a.el" "b.el" "*?")))
                 '("a.el" "b.el" "*?"))))

(ert-deftest test/2-run/result-is-correct ()
  (should (= 1.0 (distance '(0 0) '(1 0))))
  (should (= 5.0 (distance '(3 3) '(6 7))))

  (should (= 0 (fib 0)))
  (should (= 1 (fib 1)))
  (should (= 8 (fib 6))))

(ert-deftest test/3-verify/functions-are-covered ()
  (should (get 'distance 'edebug))
  (should (get 'fib 'edebug)))

(ert-deftest test/3-verify/check-distance-stop-points-number-of-covers ()
  (dolist (stop-point-covers (undercover--stop-points-covers 'distance))
    (should (= stop-point-covers 2))))

(ert-deftest test/3-verify/check-coverage-statistics ()
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

(ert-deftest test/3-verify/check-environment-variables ()
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

(ert-deftest test/3-verify/check-coveralls-report ()
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

(ert-deftest test/3-verify/should-error ()
  (with-env-variable "TRAVIS" nil
    (should-error (undercover-report))
    (should-error (undercover-coveralls--create-report))))

(ert-deftest test/3-verify/check-simplecov-report ()
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

(ert-deftest test/3-verify/check-text-report ()
  (let* ((undercover--files-coverage-statistics (make-hash-table :test 'equal))
         (undercover--files (list (file-truename "test/first-example-library/first-example-library.el")))
         (report (undercover-text--create-report)))
    (should (string-equal report "== Code coverage text report ==
first-example-library : Percent 100% [Relevant: 10 Covered: 10 Missed: 0]
"))))

(ert-deftest test/3-verify/check-text-report-file ()
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

(ert-deftest test/3-verify/check-lcov-report-file ()
  (let* ((undercover--files-coverage-statistics (make-hash-table :test 'equal))
         (undercover--files (list (file-truename "test/first-example-library/first-example-library.el")))
         (undercover--report-file-path "/tmp/undercover-lcov-report.info"))
    (when (file-readable-p undercover--report-file-path)
      (delete-file undercover--report-file-path))
    (undercover-report 'lcov)
    (let ((report (with-temp-buffer
                    (insert-file-contents undercover--report-file-path)
                    (buffer-substring-no-properties (point-min) (point-max)))))
      (should (string-equal report (format "SF:%s
DA:15,2
DA:16,2
DA:17,2
DA:18,2
DA:19,2
DA:20,2
DA:24,27
DA:25,27
DA:26,21
DA:27,12
end_of_record
" (car undercover--files)))))))

;;; first-example-library-test.el ends here
