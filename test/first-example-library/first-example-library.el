;;; first-exanple-library.el --- undercover.el: Library for test coverage tests -*- lexical-binding: t; -*-

;; Copyright (c) 2014 Sviridov Alexander

;; Author: Sviridov Alexander <sviridov.vmi@gmail.com>

;;; Commentary:

;; Library for test coverage tests.

;;; Code:

(defun distance (point-1 point-2)
  "Return distance between POINT-1 and POINT-2."
  (let ((x1 (car point-1))
        (x2 (car point-2))
        (y1 (cadr point-1))
        (y2 (cadr point-2)))
    (sqrt (+ (expt (- x1 x2) 2)
             (expt (- y1 y2) 2)))))

(defun fib (n)
  "Return N's Fibonacci number."
  (cond
    ((zerop n) 0)
    ((= n 1) 1)
    (t (+ (fib (- n 1)) (fib (- n 2))))))

(provide 'first-example-library)
;;; first-example-library.el ends here
