;;; undercover.el --- Test coverage library for Emacs -*- lexical-binding: t -*-

;; Copyright (c) 2014 Sviridov Alexander

;; Author: Sviridov Alexander <sviridov.vmi@gmail.com>
;; URL: https://github.com/sviridov/undercover.el
;; Created: Sat Sep 27 2014
;; Keywords: lisp, tests, coverage, tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (shut-up "0.3.2"))

;;; Commentary:

;; Provides a test coverage tools for Emacs packages.

;;; Code:

(eval-when-compile (require 'cl))

(require 'edebug)
(require 'json)
(require 'shut-up)

(defconst undercover-version "0.1.0")

(defvar undercover-force-coverage nil
  "If nil, test coverage check will be done only under continuous integration service.")

(defvar undercover--files nil
  "List of files for test coverage check.")

(defvar undercover--files-coverage-statistics (make-hash-table :test 'equal)
  "Table of coverage statistics for each file in `undercover--files'.")

;; Utilities

(defun undercover--fill-hash-table (hash-table &rest keys-and-values)
  "Fill HASH-TABLE from KEYS-AND-VALUES."
  (loop for (key value) on keys-and-values by #'cddr
        do (puthash key value hash-table))
  hash-table)

(defun undercover--make-hash-table (&rest keys-and-values)
  "Create new hash-table and fill it from KEYS-AND-VALUES."
  (apply #'undercover--fill-hash-table (make-hash-table) keys-and-values))

;; `edebug' related functions and hacks:

;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=6415
(def-edebug-spec cl-destructuring-bind (sexp form body))

(defun undercover--edebug-files (files)
  "Use `edebug' package to instrument all macros and functions in FILES."
  (let ((edebug-all-defs (undercover--coverage-enabled-p)))
    (save-excursion
      (dolist (file files)
        (eval-buffer (find-file file)))))
  (setq undercover--files files))

(setf (symbol-function 'undercover--stop-point-before)
      (lambda (before-index)
        "Increase number of times that stop point at BEFORE-INDEX was covered."
        (incf (aref edebug-freq-count before-index))
        before-index))

(setf (symbol-function 'undercover--stop-point-after)
      (lambda (before-index after-index value)
        "Increase number of times that stop point at AFTER-INDEX was covered."
        (incf (aref edebug-freq-count after-index))
        (undercover--align-counts-between-stop-points before-index after-index)
        value))

(setf (symbol-function 'undercover--align-counts-between-stop-points)
      (lambda (before-index after-index)
        "Decrease number of times that stop points between BEFORE-INDEX and AFTER-INDEX are covered."
        (do ((index (1+ before-index) (1+ index)))
            ((>= index after-index))
          (setf (aref edebug-freq-count index)
                (min (aref edebug-freq-count index)
                     (aref edebug-freq-count before-index))))))

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
             (previous-score (gethash line statistics cover))
             (new-score (min previous-score cover)))
        (puthash line new-score statistics)))))

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
  (save-excursion
    (find-file file)
    (undercover--fill-hash-table undercover--files-coverage-statistics
      file (undercover--file-coverage-statistics))))

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

(defun undercover--get-git-info (&rest args)
  "Execute Git with ARGS, returning the first line of its output."
  (with-temp-buffer
    (apply #'process-file "git" nil t nil "--no-pager" args)
    (goto-char (point-min))
    (buffer-substring-no-properties
     (line-beginning-position)
     (line-end-position))))

(defun undercover--get-git-info-from-log (format)
  "Get first line of Git log in given FORMAT."
  (undercover--get-git-info "log" "-1" (format "--pretty=format:%%%s" format)))

(defun undercover--get-git-remotes ()
  "Return list of Git remotes."
  (with-temp-buffer
    (process-file "git" nil t nil "--no-pager" "remote")
    (let ((remotes (split-string (buffer-string) "\n" t))
          (config-path-format (format "remote.%%s.url"))
          (remotes-info nil))
      (dolist (remote remotes remotes-info)
        (let* ((remote-url (undercover--get-git-info "config" (format config-path-format remote)))
               (remote-table (undercover--make-hash-table
                              "name" remote
                              "url"  remote-url)))
          (push remote-table remotes-info))))))

;; coveralls.io report:

(defun undercover--update-coveralls-report-with-travis-ci (report)
  "Update test coverage REPORT for coveralls.io with Travis CI service information."
  (undercover--fill-hash-table report
    "service_name"   "travis-ci"
    "service_job_id" (getenv "TRAVIS_JOB_ID")))

(defun undercover--update-coveralls-report-with-git (report)
  "Update test coverage REPORT for coveralls.io with Git information."
  (undercover--fill-hash-table report
    "git" (undercover--make-hash-table
           "branch"  (undercover--get-git-info "rev-parse" "--abbrev-ref" "HEAD")
           "remotes" (undercover--get-git-remotes)
           "head"    (undercover--make-hash-table
                      "id"              (undercover--get-git-info-from-log "H")
                      "author_name"     (undercover--get-git-info-from-log "aN")
                      "author_email"    (undercover--get-git-info-from-log "ae")
                      "committer_name"  (undercover--get-git-info-from-log "cN")
                      "committer_email" (undercover--get-git-info-from-log "ce")
                      "message"         (undercover--get-git-info-from-log "s")))))

(defun undercover--coveralls-file-coverage-report (statistics)
  "Translate file coverage STATISTICS into coveralls.io format."
  (let (file-coverage)
    (dotimes (line (count-lines (point-min) (point-max)))
      (push (gethash (1+ line) statistics) file-coverage))
    (nreverse file-coverage)))

(defun undercover--coveralls-file-report (file)
  "Create part of coveralls.io report for FILE."
  (save-excursion
    (find-file file)
    (let ((file-name (file-relative-name file (locate-dominating-file default-directory ".git")))
          (file-content (buffer-substring-no-properties (point-min) (point-max)))
          (coverage-report (undercover--coveralls-file-coverage-report
                            (gethash file undercover--files-coverage-statistics))))
      (undercover--make-hash-table
       "name"     file-name
       "source"   file-content
       "coverage" coverage-report))))

(defun undercover--fill-coveralls-report (report)
  "Fill test coverage REPORT for coveralls.io."
  (undercover--fill-hash-table report
    "source_files" (mapcar #'undercover--coveralls-file-report undercover--files)))

(defun undercover--create-coveralls-report ()
  "Create test coverage report for coveralls.io."
  (undercover--collect-files-coverage undercover--files)
  (let ((report (make-hash-table)))
    (cond
     ((undercover--under-travic-ci-p) (undercover--update-coveralls-report-with-travis-ci report))
     (t (error "Unsupported coveralls.io report")))
    (undercover--update-coveralls-report-with-git report)
    (undercover--fill-coveralls-report report)
    (json-encode report)))

(defun undercover--send-coveralls-report (json-report)
  "Send JSON-REPORT to coveralls.io."
  (save-excursion
    (let ((json-file "/tmp/json_file")
          (coveralls-url "https://coveralls.io/api/v1/jobs"))
      (shut-up
        (find-file json-file)
        (erase-buffer)
        (insert json-report)
        (save-buffer))
      (message "Sending: report to coveralls.io")
      (shell-command (format "curl -v -include --form json_file=@%s %s" json-file coveralls-url))
      (message "Sending: OK"))))

(defun undercover--coveralls-report ()
  "Create and submit test coverage report to coveralls.io."
  (undercover--send-coveralls-report (undercover--create-coveralls-report)))

;; `ert-runner' related functions:

(defun undercover--report-on-kill ()
  "Trigger `undercover-report' before exit."
  (ignore-errors
    (undercover-report)))

(defun undercover--set-ert-runner-handlers ()
  "Add `undercover-report' to `kill-emacs-hook'."
  (add-hook 'kill-emacs-hook 'undercover--report-on-kill))

;;; Main functions:

(defun undercover--coverage-enabled-p ()
  "Check that `undercover' is enabled."
  (or undercover-force-coverage (undercover--under-ci-p)))

(defun undercover-report (&optional report-type)
  "Create and submit (if needed) test coverage report based on REPORT-TYPE.
Posible values of REPORT-TYPE: coveralls."
  (case (or report-type (undercover--determine-report-type))
    (coveralls (undercover--coveralls-report))
    (t (error "Unsupported report-type"))))

;;;###autoload
(defun undercover (&rest files)
  "Enable test coverage for FILES.
If running under `ert-runner' and Travic CI automatically generate report
on `kill-emacs' and send it to coveralls.io."
  (when (undercover--coverage-enabled-p)
    (undercover--set-edebug-handlers)
    (when (getenv "ERT_RUNNER_ARGS")
      (undercover--set-ert-runner-handlers)))
  (undercover--edebug-files (mapcar #'file-truename files)))

(provide 'undercover)
;;; undercover.el ends here
