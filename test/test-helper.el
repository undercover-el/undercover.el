;;; test-helper.el --- undercover.el: Unit-test setup -*- lexical-binding: t; -*-

;; Copyright (c) 2014 Sviridov Alexander

;; Author: Sviridov Alexander <sviridov.vmi@gmail.com>

;;; Commentary:

;; Test suite setup for ERT Runner.

(require 'undercover)
(require 's)

;;; Code:

(let ((undercover-force-coverage t))
  (undercover "undercover.el")
  (load "undercover.el"))

(defadvice undercover-safe-report (around self-report activate)
  (let ((undercover--files (list (file-truename "undercover.el")))
        (undercover--send-report t)
        (undercover--report-file-path "/tmp/undercover-coverage.json"))
    ad-do-it))

(message "Running tests on Emacs %s" emacs-version)

(when (s-contains? "--win" (getenv "ERT_RUNNER_ARGS"))
  (defun ert-runner/run-tests-batch-and-exit (selector)
    (ert-run-tests-interactively selector)))

;;; test-helper.el ends here
