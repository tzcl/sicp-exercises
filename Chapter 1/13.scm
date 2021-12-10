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

(define (cube x) (* x x x))

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

;; Exercise 1.35
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point cos 1.0)
;; => 0.7390822985224024

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) ; golden ratio
;; => 1.6180327868852458
(define phi (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

;; Exercise 1.36
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
;; without average damping: takes 34 steps (with a precision of 1e-5)
(fixed-point (lambda (x) (/ (+ (log 1000) (* x (log x))) (* 2 (log x)))) 2.0)
;; with average damping: takes 9 steps

;; Exercise 1.37
;; Tricky -- quite a bit of discussion on schemewiki
(define (cont-frac n d k)
  (define (helper i)
    (if (> i k) 0
        (/ (n i) (+ (d i) (helper (1+ i))))))
  (helper 1))

(define (cont-frac-iter n d k)
  (define (iter acc i)
    (if (= i 0) acc
        (iter (/ (n i) (+ (d i) acc)) (1- i))))
  (iter 0 k))

;; Approximate 1/phi
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           12)                          ; need k = 12
(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                12)

;; Exercise 1.38
;; Approximate e-2
(cont-frac (lambda (i) 1.0)
           (lambda (i)
             (if (= (remainder (1+ i) 3) 0)
                 (/ (* (1+ i) 2) 3)
                 1.0))
           50)

;; Exercise 1.39
(define (tan-cf x k)
  (define (n i)
    (let ((x2 (expt x 2)))
      (if (= i 1) x
          (- x2))))
  (define (d i)
    (1- (* 2.0 i)))
  (cont-frac n d k))

;; Exercise 1.40
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define dx 0.00001)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (expt x 3)
                 (* a (expt x 2))
                 (* b x)
                 c)))

;; Exercise 1.41
(define (twice f)
  (lambda (x) (f (f x))))

(((twice (twice twice)) 1+) 5)
;; => 21

;; Exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;; Exercise 1.43
(define (repeat f n)
  (if (= n 1) (lambda (x) (f x))
      (compose f (repeat f (1- n)))))

;; Repeating inc 5 times should be the same as x + 5
((repeat 1+ 5) 5)
;; => 10

;; Exercise 1.44
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

;; We can get the n-fold smoothed function using
;; ((repeat smooth n) f)

;; Exercise 1.45
(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2.0)))

;; Trying to find the 4th root doesn't work with one average-dampen
;; Instead, we need to repeat log2(n) times
(define (nth-root n x)
  (define (reps n)                      ; could instead use (floor (log2 n))
    (define (iter acc)
      (if (< (- n (* 2 acc)) 0) acc
          (iter (* 2 acc))))
    (iter 1))
  (fixed-point ((repeat average-damp (reps n)) (lambda (y) (/ x (expt y (1- n))))) 1.0))

;; Exercise 1.46
;; We can generalise some of the numerical methods we've seen as
;; iterative improvement.
(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess) guess
        (iter (improve guess))))
  iter)

(define (sqrt x)
  ((iterative-improve
    (lambda (y)
      (< (abs (- (expt y 2) x)) tolerance))
    (average-damp
     (lambda (y) (/ x y))))
   1.0))

(define (fixed-point f)
  ((iterative-improve
    (lambda (y)
      (< (abs (- y (f y))) tolerance))
    (lambda (y)
      (f y))) 2.0))
