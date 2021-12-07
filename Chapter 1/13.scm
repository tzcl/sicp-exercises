;; Exercise 1.29
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (yk k) (f (+ a (* k h))))
  (define (term k)
    (cond ((or (= k 0) (= k n)) (yk k))
          ((odd? k) (* 4 (yk k)))
          (else (* 2 (yk k)))))
  (* (/ h 3.0)
     (sum term 0 1+ n)))
;; Got confused with a and b (the a and b in simpsons-rule are fixed whereas the
;; a and b in sum range from 0 to n)

(integral cube 0 1 0.01)
;; => 0.24998750000000042
(integral cube 0 1 0.001)
;; => 0.249999875000001

(simpsons-rule cube 0 1 100)
;; => 0.25
(simpsons-rule cube 0 1 1000)
;; => 0.25

;; Exercise 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(sum-iter cube 0 1+ 10)
;; => 3025
(sum cube 0 1+ 10)
;; => 3025

;; Exercise 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(product identity 1 1+ 5)
;; => 120
(product-iter identity 1 1+ 5)
;; => 120

(define (factorial n)
  (product identity 1 1+ n))

(factorial 10)
;; => 3628800

(define (approx-pi n)
  (define (term n)
    (if (even? n)
        (/ (+ n 2) (+ n 1))
        (/ (+ n 1) (+ n 2))))
  (* (product term 1 1+ n) 4.0))

(approx-pi 1000)
;; => 3.1431607055322663

;; Exercise 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b) null-value
      (combiner (term a)
          (accumulate combiner null-value
                      term (next a) next b))))

(define (accum-iter op start term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (op (term a) result))))
  (iter a start))

(define (sum term a next b)
  (accum-iter + 0 term a next b))

(define (product term a next b)
  (accum-iter * 1 term a next b))

(sum identity 1 1+ 10)
;; => 55
(product identity 1 1+ 5)
;; => 120

;; Exercise 1.33
(define (filter-accum filter op null-value term a next b)
  (if (> a b) null-value
      (op (if (filter a) (term a)
              null-value)
          (filter-accum filter op null-value term (next a) next b))))

(filter-accum odd? + 0 identity 1 1+ 10)
;; => 25
