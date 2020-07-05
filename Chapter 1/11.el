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

(provide '11)
;;; 11.el ends here
