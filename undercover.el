;;; undercover.el --- Test coverage library for Emacs -*- lexical-binding: t -*-

;; Copyright (c) 2014 Sviridov Alexander

;; Author: Sviridov Alexander <sviridov.vmi@gmail.com>
;; URL: https://github.com/sviridov/undercover.el
;; Created: Sat Sep 27 2014
;; Keywords: lisp, tests, coverage, tools
;; Version: 0.0.1

;;; Commentary:

;; Provides a test coverage tools for Emacs packages.

(eval-when-compile (require 'cl))

(require 'edebug)

;;; Code:

(defun undercover--edebug-files (files)
  "Use `edebug' package to instrument all macros and functions in FILES."
  (let ((edebug-all-defs t))
    (dolist (file files) ;; FIXME: need shut-up.el or analogue for edebug
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

(defun undercover--set-edebug-handlers ()
  "Replace `edebug-before' and `edebug-after' functions with `undercover' handlers."
  (defalias 'edebug-before 'undercover--stop-point-before)
  (defalias 'edebug-after 'undercover--stop-point-after))

;;;###autoload
(defun undercover (&rest files)
  "Use `edebug' package to instrument all macros and functions in FILES.
Replace `edebug-before' and `edebug-after' functions with `undercover' handlers."
  (undercover--edebug-files files)
  (undercover--set-edebug-handlers))

(provide 'undercover)
;;; undercover.el ends here
