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

;; Exercise 1.11
(defun recur-f (n)
  (if (< n 3)
      n
    (+ (recur-f (- n 1)) (* 2 (recur-f (- n 2))) (* 3 (recur-f (- n 3))))))

(recur-f 2)
(recur-f 3)
(recur-f 4)

(defun f (n)
  (if (< n 3)
      n
    (iter-f 2 1 0 n)))
(defun iter-f (a b c n)
  (if (< n 3)
      a
    (iter-f (+ a (* 2 b) (* 3 c)) a b (- n 1))))

(f 2)
(f 3)
(f 4)

;; Exercise 1.12
(defun pascal (n m)
  "The Mth element of the Nth row of Pascal's triangle."
  (cond ((= n 0) 1)
        ((or (= m 0) (= m n)) 1)
        (t (+ (pascal (1- n) (1- m)) (pascal (1- n) m)))))

(pascal 0 0)
(pascal 1 1)
(pascal 2 2)
(pascal 4 2)

;; Exercise 1.16
;; Helper functions
(defun even? (n)
  (= (mod n 2) 0))
(defun square (n)
  (* n n))

;; This exp is logarithmic (exponent decreases by a factor of 2)
(defun fast-exp (b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-exp b (/ n 2))))
        (t (* b (fast-exp b (1- n))))))

;; Design an iterative version of fast-exp
;; Idea: use a as a state variable / accumulator
(defun iter-exp (b n)
  (defun iter (a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (t (iter (* a b) b (1- n)))))
  (iter 1 b n))

(iter-exp 2 3)
(iter-exp 2 4)
(iter-exp 2 8)
(iter-exp 2 9)

;; Exercise 1.17
(defun fast-mul (a b)
  "Multiply A * B with log(B) number of operations."
  (cond ((= b 1) a)
        ((even? b) (* 2 (fast-mul a (/ b 2))))
        (t (+ a (fast-mul a (1- b))))))

(fast-mul 2 3)
(fast-mul 5 13)

;; Exercise 1.18
(defun iter-mul (a b)
  (defun iter (x a b)
    (cond ((= b 1) x)
          ((even? b) (iter (+ x a) (* 2 a) (/ b 2)))
          (t (iter (+ x a) a (1- b)))))
  (iter a a b))

(iter-mul 2 2)
(iter-mul 2 3)
(iter-mul 2 4)
(iter-mul 2 5)
(iter-mul 5 13)

;; Exercise 1.19
(defun log-fib (n)
  (defun p* (p q) (+ (square p) (square q)))
  (defun q* (p q) (+ (square q) (* 2 p q)))
  (defun iter (a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (iter a b (p* p q) (q* p q) (/ count 2)))
          (t (iter (+ (* b q) (* a q) (* a p))
                   (+ (* b p) (* a q))
                   p q (1- count)))))
  (iter 1 0 0 1 n))

(log-fib 0)
(log-fib 1)
(log-fib 2)
(log-fib 3)
(log-fib 4)
(log-fib 5)

;; Exercise 1.21
(defun smallest-divisor (n)
  (defun find-divisor (n test-divisor)
    (defun divides? (a b)
      (= (mod b a) 0))
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (t (find-divisor n (1+ test-divisor)))))
  (find-divisor n 2))

(smallest-divisor 199)
;; => 199
(smallest-divisor 1999)
;; => 1999
(smallest-divisor 19999)
;; => 7

;; Exercise 1.22
(defun correct-prime? (n)
  "Conclusively check whether N is prime (in log N time)."
  (= n (smallest-divisor n)))

(defun timed-prime-test (n)
  (message "%d" n)
  (start-prime-test n (float-time)))

(defun start-prime-test (n start-time)
  (if (correct-prime? n)
      (report-prime (- (float-time) start-time))))

(defun report-prime (elapsed-time)
  (message " *** ")
  (message "%f" elapsed-time))

(timed-prime-test 89)

(defun expmod (base ex m)
  (cond ((= ex 0) 1)
        ((even? ex) (mod (square (expmod base (/ ex 2) m)) m))
        (t (mod (* base (expmod base (1- ex) m)) m))))

(defun fermat-test (n)
  (defun try (a)
    (= (expmod a n n) a))
  (try (1+ (random (1- n)))))

(defun fast-prime? (n times)
  (cond ((= times 0) t)
        ((fermat-test n) (fast-prime? n (1- times)))
        (t nil)))

(provide '12)
;;; 12.el ends here
