;;; 11.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Toby Law
;;
;; Author: Toby Law <http://github/toby>
;; Maintainer: Toby Law <toby@tzcl.me>
;; Created: July 06, 2020
;; Modified: July 06, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/toby/11
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Exercises for 1.1 The Elements of Programming
;;
;;; Code:

;; Exercise 1.2
;; / performs floating-point division only if at least one of its arguments is a float
(/ (+ 5
      4
      (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3.0 ; cast to float
      (- 6 2)
      (- 2 7)))

;; Exercise 1.3
(defun square (x) (* x x))

(defun sum-of-squares (x y)
  (+ (square x) (square y)))

(defun sum-squares-largest (x y z)
  (cond
   ((and (>= x y) (>= y z)) (sum-of-squares x y))
   ((and (>= y x) (>= z x)) (sum-of-squares y z))
   ((and (>= x y) (>= z y)) (sum-of-squares x z))))

(sum-squares-largest 1 2 3)

;; Exercise 1.4
(defun a-plus-abs-b (a b)
  "In Scheme you would write ((if (> b 0) + -) a b)
but we need to use funcall in elisp."
  (funcall (if (> b 0) #'+ #'-) a b))

(a-plus-abs-b 1  2) ; 3
(a-plus-abs-b 1 -2) ; 3

;; Exercise 1.5
(defun (p) (p))
(defun test (x y)
  (if (= x 0) 0 y))

;; Using applicative-order evaluation
;; (evaluate operator and operands and apply the
;; resulting procedure to the resulting arguments)
(test 0 (p))
(test 0 (p))
(test 0 (p)) ; gets stuck trying to expand p

;; Using normal-order evaluation
;; (evaluates procedure then arguments)
(test 0 (p))
(if (= 0 0) 0 (p)) ; 0

(provide '11)
;;; 11.el ends here
