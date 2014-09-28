;;; test-helper.el --- undercover.el: Unit-test suite -*- lexical-binding: t; -*-

;; Copyright (c) 2014 Sviridov Alexander

;; Author: Sviridov Alexander <sviridov.vmi@gmail.com>

;;; Commentary:

;; The unit test suite of undercover.el.

;;; Code:

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
  (let ((example-library-statistics (gethash (file-truename "test/example-library.el")
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

;;; undercover-test.el ends here
