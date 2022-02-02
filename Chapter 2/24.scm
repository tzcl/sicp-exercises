;; Complex number operations
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

;; Rectangular form
(define (make-from-real-imag-rect x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rect r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

(define (real-part-rect z) (car z))
(define (imag-part-rect z) (cdr z))
(define (magnitude-rect z)
  (sqrt (+ (expt (real-part-rect z) 2)
           (expt (imag-part-rect z) 2))))
(define (angle-rect z)
  (atan (imag-part-rect z) (real-part-rect z)))

;; Polar form
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar (cons (sqrt (+ (expt x 2) (expt y 2)))
                           (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))

;; Tagging data
(define (attach-type type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum) (car datum)
      (error "Bad typed datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum) (cdr datum)
      (error "Bad typed datum: CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

;; Generic selectors
(define (real-part z)
  (cond ((rectangular? z) (real-part-rect (contents z)))
        ((polar? z) (real-part-polar (contents z)))
        (else (error "Invalid type" z))))
(define (imag-part z)
  (cond ((rectangular? z) (imag-part-rect (contents z)))
        ((polar? z) (imag-part-polar (contents z)))
        (else (error "Invalid type" z))))
(define (magnitude z)
  (cond ((rectangular? z) (magnitude-rect (contents z)))
        ((polar? z) (magnitude-polar (contents z)))
        (else (error "Invalid type" z))))
(define (angle z)
  (cond ((rectangular? z) (angle-rect (contents z)))
        ((polar? z) (angle-polar (contents z)))
        (else (error "Invalid type" z))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rect x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;; Assume we have the operations:
;;   (put op types item): puts an entry in the table
;;   (get op types):      retrieves an entry from the table
;; We see how to implement these in Section 3.3.3
;; We can then apply an operation from this table using:
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc (apply proc (map contents args))
          (error "No method for these types" (list op type-tags))))))

;; Exercise 2.73
;; Assume we are representing expressions as lists in prefix notation
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; a) We've changed the deriv procedure so instead of having different clauses
;; for different types of expressions, we're using a dispatch table that looks
;; like this:
;;
;;        '+     '*
;; deriv  ...    ...
;;
;; Therefore, it doesn't make sense to include number? and variable? since there
;; is nothing to dispatch on for them (there is no operator that tells us which
;; procedure to apply), rather they examine properties of the data itself

;; b)
;; Assume everything is bracketed properly
(define (install-deriv-sum)
  (define (make-sum e1 e2) (list '+ e1 e2))
  (define (addend e) (car e))
  (define (augend e) (cadr e))

  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  (put 'deriv '+ deriv-sum))

(define (install-deriv-product)
  (define (make-sum e1 e2) (list '+ e1 e2))
  (define (make-product e1 e2) (list '* e1 e2))
  (define (multiplier e) (car e))
  (define (multiplicand e) (cadr e))

  (define (deriv-product exp var)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))

  (put 'deriv '* deriv-product))

;; c)
(define (install-deriv-expt)
  (define (make-product e1 e2) (list '* e1 e2))
  (define (make-exponent base exp) (list '** base exp))
  (define (base e) (car e))
  (define (expt e) (cadr e))

  (define (deriv-expt exp var)
    (make-product
     (expt exp)
     (make-product (make-exponent (base exp) (1- (expt exp)))
                   (deriv (base exp) var))))

  (put 'deriv '** deriv-expt))

;; d) Changing the dispatch line involves changing all the `put` statements
;; to reflect the new structure of the table.

;; Exercise 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Invalid operation" op))))
  dispatch)

;; Exercise 2.76
;; Often add new types: data-directed programming or message parsing since
;; adding a new type will require only adding a new package or adding a
;; new object that accepts the same messages. The upper level calling code
;; won't need to be changed.
;;
;; Often add new operations: generic operations or data-directed programming
;; because adding a new operation will require adding a new generic function
;; or a new package.
