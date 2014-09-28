;;; undercover.el --- Test coverage library for Emacs -*- lexical-binding: t -*-

;; Copyright (c) 2014 Sviridov Alexander

;; Author: Sviridov Alexander <sviridov.vmi@gmail.com>
;; URL: https://github.com/sviridov/undercover.el
;; Created: Sat Sep 27 2014
;; Keywords: lisp, tests, coverage, tools
;; Version: 0.0.1
;; Package-Requires: ((shut-up "0.3.2"))

;;; Commentary:

;; Provides a test coverage tools for Emacs packages.

(eval-when-compile (require 'cl))

(require 'edebug)
(require 'json)
(require 'shut-up)

;;; Code:

(defvar undercover-force-coverage nil
  "If nil, test coverage check will be done only under continuous integration service.")

(defvar undercover--files nil
  "List of files for test coverage check.")

(defvar undercover--files-coverage-statistics (make-hash-table :test 'equal)
  "Table of coverage statistics for each file in `undercover--files'.")

;; Edebug related functions:

(defun undercover--edebug-files (files)
  "Use `edebug' package to instrument all macros and functions in FILES."
  (let ((edebug-all-defs (undercover--coverage-enabled-p)))
    (dolist (file files)
      (eval-buffer (find-file file))))
  (setq undercover--files files))

(defun undercover--stop-point-before (before-index)
  "Increase number of times that stop point at BEFORE-INDEX was covered."
  (incf (aref edebug-freq-count before-index))
  before-index)

(defun undercover--stop-point-after (_before-index after-index value)
  "Increase number of times that stop point at AFTER-INDEX was covered."
  (incf (aref edebug-freq-count after-index))
  value)

(defun undercover--stop-points (name)
  "Return stop points ordered by position for NAME."
  (coerce (nth 2 (get name 'edebug)) 'list))

(defun undercover--stop-points-covers (name)
  "Return number of covers for each stop point ordered by position for NAME."
  (coerce (get name 'edebug-freq-count) 'list))

(defun undercover--shut-up-edebug-message ()
  "Muffle `edebug' message \"EDEBUG: function\"."
  (defadvice edebug-make-form-wrapper (around undercover-shut-up activate)
    (shut-up ad-do-it)))

(defun undercover--set-edebug-handlers ()
  "Replace and advice some `edebug' functions with `undercover' handlers."
  (defalias 'edebug-before 'undercover--stop-point-before)
  (defalias 'edebug-after 'undercover--stop-point-after)
  (undercover--shut-up-edebug-message))

;; Coverage statistics related functions:

(defun undercover--symbol-coverage-statistics (edebug-symbol statistics)
  "Collect coverage statistics for EDEBUG-SYMBOL into STATISTICS hash."
  (let* ((start-marker (car (get edebug-symbol 'edebug)))
         (points (undercover--stop-points edebug-symbol))
         (points-covers (undercover--stop-points-covers edebug-symbol))
         (points-and-covers (map 'list #'cons points points-covers)))
    (dolist (point-and-cover points-and-covers)
      (let* ((point (car point-and-cover))
             (line  (line-number-at-pos (+ point start-marker)))
             (cover (cdr point-and-cover))
             (previous-score (gethash line statistics)))
        (setf (gethash line statistics)
              (if previous-score (min previous-score cover) cover))))))

(defun undercover--file-coverage-statistics ()
  "Collect coverage statistics for current-file into hash.
Keys of that hash are line numbers.
Values of that hash are number of covers."
  (let ((statistics (make-hash-table)))
    (dolist (edebug-data edebug-form-data)
      (let ((edebug-symbol (car edebug-data)))
        (when (get edebug-symbol 'edebug)
          (undercover--symbol-coverage-statistics edebug-symbol statistics))))
    statistics))

(defun undercover--collect-file-coverage (file)
  "Collect coverage statistics for FILE."
  (find-file file)
  (setf (gethash file undercover--files-coverage-statistics)
        (undercover--file-coverage-statistics)))

(defun undercover--collect-files-coverage (files)
  "Collect coverage statistics for each file in FILES."
  (dolist (file files)
    (undercover--collect-file-coverage file)))

;; Continuous integration related functions:

(defun undercover--under-travic-ci-p ()
  "Check that `undercover' running under Travis CI service."
  (getenv "TRAVIS"))

(defun undercover--under-ci-p ()
  "Check that `undercover' running under continuous integration service."
  (undercover--under-travic-ci-p))

;;; Reports related functions:

(defun undercover--determine-report-type ()
  "Automatic report-type determination."
  (and (undercover--under-ci-p) 'coveralls))

;; Coveralls report:

(defun undercover--update-coveralls-report-for-travis-ci (report)
  "Update test coverage REPORT for coveralls.io with Travis CI service information."
  (setf (gethash "service_name" report) "travis-ci"
        (gethash "service_job_id" report) (getenv "TRAVIS_JOB_ID")))

(defun undercover--coveralls-file-coverage-report (statistics)
  "Translate file coverage STATISTICS into coveralls.io format."
  (let (file-coverage)
    (dotimes (line (count-lines (point-min) (point-max)))
      (push (gethash (1+ line) statistics) file-coverage))
    (reverse file-coverage)))

(defun undercover--coveralls-file-report (file)
  "Create part of coveralls.io report for FILE."
  (find-file file)
  (let ((report (make-hash-table)))
    (setf (gethash "name" report) file
          (gethash "source" report) (buffer-string)
          (gethash "coverage" report)
          (undercover--coveralls-file-coverage-report (gethash file undercover--files-coverage-statistics)))
    report))

(defun undercover--fill-coveralls-report (report)
  "Fill test coverage REPORT for coveralls.io."
  (setf (gethash "source_files" report)
        (mapcar #'undercover--coveralls-file-report undercover--files)))

(defun undercover--create-coveralls-report ()
  "Create test coverage report for coveralls.io."
  (let ((report (make-hash-table)))
    (cond
     ((undercover--under-travic-ci-p) (undercover--update-coveralls-report-for-travis-ci report))
     (t (error "Unsupported coveralls report")))
    (undercover--fill-coveralls-report report)
    (json-encode report)))

(defun undercover--send-coveralls-report (value) value)

(defun undercover--coveralls-report ()
  "Create and submit test coverage report to coveralls.io."
  (undercover--send-coveralls-report (undercover--create-coveralls-report)))

;;; Main functions:

(defun undercover--coverage-enabled-p ()
  "Check that `undercover' is enabled."
  (or undercover-force-coverage (undercover--under-ci-p)))

(defun undercover-report (&optional report-type)
  "Create and submit (if needed) test coverage report based on REPORT-TYPE.
Posible values of REPORT-TYPE: coveralls."
  (undercover--collect-files-coverage undercover--files)
  (case (or report-type (undercover--determine-report-type))
    (coveralls (undercover--coveralls-report))
    (t (error "Unsupported report-type"))))

;;;###autoload
(defun undercover (&rest files)
  "FIXME: awesome documentation"
  (when (undercover--coverage-enabled-p)
    (undercover--set-edebug-handlers))
  (undercover--edebug-files (mapcar #'expand-file-name files)))

(provide 'undercover)
;;; undercover.el ends here
