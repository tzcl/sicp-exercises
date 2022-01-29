(define (deriv exp var)
  (cond ((number? exp) 0)
        ((var? exp) (if (same-var? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else (error "Unknown expression type: DERIV" exp))))

(define (var? x) (symbol? x))
(define (same-var? v1 v2)
  (and (var? v1) (var? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum e1 e2)
  (cond ((=number? e1 0) e2)
        ((=number? e2 0) e1)
        ((and (number? e1) (number? e2)) (+ e1 e2))
        (else (list '+ e1 e2))))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (make-product e1 e2)
  (cond ((or (=number? e1 0) (=number? e2 0)) 0)
        ((=number? e1 1) e2)
        ((=number? e2 1) e1)
        ((and (number? e1) (number? e2)) (* e1 e2))
        (else (list '* e1 e2))))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

;; Exercise 2.56
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((var? exp) (if (same-var? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponent? exp)
         (make-product
          (exponent exp)
          (make-product (make-exponent (base exp) (1- (exponent exp)))
                        (deriv (base exp) var))))
        (else (error "Unknown expression type: DERIV" exp))))

(define (make-exponent base ex)
  (cond ((=number? ex 0) 1)
        ((=number? base 1) 1)
        ((=number? ex 1) base)
        ((and (number? base) (number? ex)) (expt base ex))
        (else (list '** base ex))))
(define (exponent? x) (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

;; Exercise 2.57
(define (augend s)
  (let ((rest (cddr s)))
    (if (null? (cdr rest)) (car rest) (cons '+ rest))))

(define (multiplicand p)
  (let ((rest (cddr p)))
    (if (null? (cdr rest)) (car rest) (cons '* rest))))

;; Exercise 2.58
;; Part A
(define (make-sum e1 e2)
  (cond ((=number? e1 0) e2)
        ((=number? e2 0) e1)
        ((and (number? e1) (number? e2)) (+ e1 e2))
        (else (list e1 '+ e2))))
(define (sum? s) (and (pair? s) (eq? (cadr s) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))

(define (make-product e1 e2)
  (cond ((or (=number? e1 0) (=number? e2 0)) 0)
        ((=number? e1 1) e2)
        ((=number? e2 1) e1)
        ((and (number? e1) (number? e2)) (* e1 e2))
        (else (list e1 '* e2))))
(define (product? p) (and (pair? p) (eq? (cadr p) '*)))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

(define (make-exponent base ex)
  (cond ((=number? ex 0) 1)
        ((=number? base 1) 1)
        ((=number? ex 1) base)
        ((and (number? base) (number? ex)) (expt base ex))
        (else (list base '** ex))))
(define (exponent? x) (and (pair? x) (eq? (cadr x) '**)))
(define (base e) (car e))
(define (exponent e) (caddr e))

;; Part B
;; Infix notation and operator precedence
;; See http://community.schemewiki.org/?sicp-ex-2.58
;; https://github.com/danielpi/SICP-Exercises/blob/master/Racket/Ex%202.58b%20Infix%20Hard.rkt

;; memq returns sublist containing sym if it matches sym (otherwise #f)
(define (memp-append sym lst)
  (define (iter lst res)
    (cond ((null? lst) #f)
          ((eq? (car lst) sym) res)
          (else (iter (cdr lst) (append res (list (car lst)))))))
  (iter lst '()))

(define (memp sym lst)
  (define (iter lst res)
    (cond ((null? lst) #f)
          ((eq? (car lst) sym) (res '()))
          (else (iter (cdr lst)
                      (lambda (x) (res (cons (car lst) x)))))))
  (iter lst (lambda (x) x)))

(define (get-exp exp) (if (null? (cdr exp)) (car exp) exp))
(define (split-right sym exp) (get-exp (cdr (memq sym exp))))
(define (split-left sym exp) (get-exp (memp sym exp)))

(define (make-sum e1 e2)
  (cond ((=number? e1 0) e2)
        ((=number? e2 0) e1)
        ((and (number? e1) (number? e2)) (+ e1 e2))
        (else (list e1 '+ e2))))
(define (sum? s) (if (memq '+ s) #t #f))
(define (addend s) (split-left '+ s))
(define (augend s) (split-right '+ s))

(define (make-product e1 e2)
  (cond ((or (=number? e1 0) (=number? e2 0)) 0)
        ((=number? e1 1) e2)
        ((=number? e2 1) e1)
        ((and (number? e1) (number? e2)) (* e1 e2))
        (else (list e1 '* e2))))
(define (product? p) (if (memq '* p) #t #f))
(define (multiplier p) (split-left '* p))
(define (multiplicand p) (split-right '* p))

(define (make-exponent base ex)
  (cond ((=number? ex 0) 1)
        ((=number? base 1) 1)
        ((=number? ex 1) base)
        ((and (number? base) (number? ex)) (expt base ex))
        (else (list base '** ex))))
(define (exponent? x) (if (memq '** x) #t #f))
(define (base e) (split-left '** e))
(define (exponent e) (split-right '** e))

;; This is inefficient since it needs to traverse the expression multiple times.
;; A better solution would be to implement some kind of recursive-descent parser
;; but I think that's overkill for this exercise.
