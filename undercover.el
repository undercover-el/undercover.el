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
(require 'shut-up)

;;; Code:

(defun undercover--edebug-files (files)
  "Use `edebug' package to instrument all macros and functions in FILES."
  (let ((edebug-all-defs t))
    (dolist (file files)
      (kill-buffer (eval-buffer (find-file file))))))

(defun undercover--stop-point-before (before-index)
  "Increase number of times that stop point at BEFORE-INDEX was covered."
  (incf (aref edebug-freq-count before-index))
  before-index)

(defun undercover--stop-point-after (_before-index after-index value)
  "Increase number of times that stop point at AFTER-INDEX was covered."
  (incf (aref edebug-freq-count after-index))
  value)

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

;;;###autoload
(defun undercover (&rest files)
  "FIXME: awesome documentation"
  (undercover--set-edebug-handlers)
  (undercover--edebug-files files))

(provide 'undercover)
;;; undercover.el ends here
