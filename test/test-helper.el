;;; test-helper.el --- undercover.el: Unit-test setup -*- lexical-binding: t; -*-

;; Copyright (c) 2014 Sviridov Alexander

;; Author: Sviridov Alexander <sviridov.vmi@gmail.com>

;;; Commentary:

;; Test suite setup for ERT Runner.

(require 'undercover)

;;; Code:

(message "Running tests on Emacs %s" emacs-version)

(when (s-contains? "--win" (getenv "ERT_RUNNER_ARGS"))
  (defun ert-runner/run-tests-batch-and-exit (selector)
    (ert-run-tests-interactively selector)))

;;; test-helper.el ends here
