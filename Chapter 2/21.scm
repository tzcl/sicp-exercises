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
(define (cons-proc x y)
  (lambda (m) (m x y)))
(define (car-proc z)
  (z (lambda (p q) p)))
(define (cdr-proc z)
  (z (lambda (p q) q)))

;; Exercise 2.5
(define (cons-ints x y)
  (* (expt 2 x) (expt 3 y)))

(define (count-divisions n x)
  (if (= (remainder n x) 0)
      (1+ (count-divisions (/ n x) x))
      0))

(define (car-ints z)
  (count-divisions z 2))
(define (cdr-ints z)
  (count-divisions z 3))

;; Exercise 2.6
;;
;; We can represent nonnegative integers using functions
;; (this is known as Church numerals)
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; (define one (add-1 zero))
;; (add-1 zero)
;; (lambda (f) (lambda (x) (f ((zero f) x))))
;; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
;; (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
;; (lambda (f) (lambda (x) (f x)))
;;
;; Likewise
;; (define two (add-1 one))
;; (add-1 one)
;; ...
;; (lambda (f) (lambda (x) (f (f x))))

(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

(define (print-cn n)
  (display ((n 1+) 0)) (newline))

;; Implementing interval arithmetic
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

;; Exercise 2.7
(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

;; Exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; Exercise 2.10
(define (div-interval x y)
  (cond
   ((= 0 (lower-bound y)) (error "Lower bound of y is 0!"))
   ((= 0 (upper-bound y)) (error "Upper bound of y is 0!"))
   (else (mul-interval x
                       (make-interval (/ 1.0 (lower-bound y))
                                      (/ 1.0 (upper-bound y)))))))

;; Alternate implementation using center + widths
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; Exercise 2.12
(define (make-center-percent c p)
  (let ((w (/ (* c p) 2)))
    (make-interval (- c w) (+ c w))))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (percent i)
  (if (= (center i) 0) 0
      (/ (- (upper-bound i) (lower-bound i)) (center i))))

;; Exercise 2.13
(define (mul-percent-interval x y)
  (let ((c1 (center x))
        (p1 (percent x))
        (c2 (center y))
        (p2 (percent y)))
    (make-center-percent (* c1 c2) (+ p1 p2))))
;; p1*p2 is negligible for small percentages

;; Issues with computing intervals
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

;; Exercise 2.14
(define interval-A (make-center-percent 10 0.01))
(define interval-B (make-center-percent 10 0.01))

(define A/A (div-interval interval-A interval-A))
(center A/A)
;; => 1.000050001250031
(percent A/A)
;; => 0.01999950001249991

(define A/B (div-interval interval-A interval-B))
(center A/B)
;; => 1.000050001250031
(percent A/B)
;; => 0.01999950001249991

;; Lem is right because interval arithmetic does not function the same as normal
;; arithmetic. Specifically, division does not work as expected since there is
;; no identity interval (see A/A != 1). Therefore, where we expect an interval
;; to be 1, we increase uncertainty.

;; Exercise 2.15
;;
;; Program 2 is better because the interval (1 1) has no uncertainty (dividing
;; this by itself gives 1 as expected). It avoids the additional uncertainty
;; program 1 incurs.

;; Exercise 2.16
;;
;; This is very difficult, especially once we have intervals that consist of
;; multiple variables. The core issue is that interval arithmetic is not a
;; field. One workaround is to use numerical methods such as MC to compute
;; intervals.
;;
;; See
;;   https://stackoverflow.com/questions/14130878/sicp-2-16-interval-arithmetic-scheme/14131196#14131196
;;   http://wiki.drewhess.com/wiki/SICP_exercise_2.16
;;
;; In general, designing an interval arithmetic system is very difficult.
