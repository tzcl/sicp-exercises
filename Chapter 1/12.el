;;; 12.el --- Processes and the Procedures They Generate -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Toby Law
;;
;; Maintainer: Toby Law <toby@tzcl.me>
;; Created: November 24, 2021
;;
;;; Commentary:
;;
;;  Section 1.2
;;  Processes and the Procedures They Generate
;;
;;; Code:

;; Exercise 1.10
;; The Ackermann function
;; Grows exceptionally quickly!
(defun A (x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        ((A (- x 1) (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

(defun f (n) (A 0 n))                   ; Same as 2n
(defun g (n) (A 1 n))                   ; Same as 2^n
;; (A 1 n)
;; (A 0 (A 1 (- n 1)))
;; (* 2 (A 1 (- n 1)))
;; (* 2 (A 0 (A 1 (- n 2))))

(defun h (n) (A 2 n))                   ; Same as 2^2 n times (n > 1)
;; n: 0 1 2 3  4
;; A: 0 2 4 16 65536
;;
;; (A 2 3)
;; (A 1 (A 2 2))
;; (A 1 (A 1 (A 2 1))) -- we can recognise this as g(n)!
;; (A 1 (A 1 2))       -- 2^(2^2)
;;
;; (A 2 4)
;; (A 1 (A 2 3))
;; From above, we have 2^(2^(2^2)) = 2^16 = 65536


(provide '12)
;;; 12.el ends here
