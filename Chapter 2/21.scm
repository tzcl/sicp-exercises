;; Implementing rational numbers
;;
;; Operations on rationals
;; (note we haven't defined rationals themselves yet!)
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

;; We need the primitives cons, car, cdr to define rationals
(define make-rat cons)
(define numer car)
(define denom cdr)

;; Use gcd to simplify rationals
(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))
  
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

;; Exercise 2.1
;; Extend make-rat to deal with negative inputs
(define (sgn x)
  (cond ((negative? x) -1)
        ((positive? x) 1)
        (else 0)))

(define (make-rat n d)
  (let* ((sign (* (sgn n) (sgn d)))
         (n (abs n))
         (d (abs d))
         (g (gcd n d)))
    (cons (* sign (/ n g)) (/ d g))))

;; Exercise 2.2
(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline))

(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment s)
  (let ((start (start-segment s))
        (end (end-segment s)))
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))

;; Exercise 2.4
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

;; Exercise 2.5
(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (count-divisions n x)
  (if (= (remainder n x) 0)
      (1+ (count-divisions (/ n x) x))
      0))

(define (car z)
  (count-divisions z 2))
(define (cdr z)
  (count-divisions z 3))
