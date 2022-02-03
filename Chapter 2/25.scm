;; Exercise 2.77
;; apply-generic is invoked twice, first dispatching magnitude for complex and
;; then dispatching magnitude for rectangular

;; Exercise 2.78
(define (attach-tag type-tag contents)
  (if (number? contents) contents (cons type-tag contents)))
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad type" datum))))
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad type" datum))))

;; Exercise 2.79
(define (equ? x y) (apply-generic 'equ? x y))

;; in install-scheme-number-package
(put 'equ? '(scheme-number scheme-number)
     (lambda (x y) (= x y)))

;; in install-rational-package
(define (equ? x y)
  (and (= (numer x) (numer y))
       (= (denom x) (denom y))))
(put 'equ? '(rational rational) equ?)

;; in install-complex-package
(define (equ? x y)
  (and (= (real-part x) (real-part y))
       (= (imag-part x) (imag-part y))))
(put 'equ? '(complex complex) equ?)

;; Exercise 2.80
(define (=zero? x) (apply-generic '=zero? x))

;; in install-scheme-number-package
(put '=zero? '(scheme-number) (lambda (x) (= x 0)))

;; in install-rational-package
(define (=zero? x) (= (numer x) 0))
(put '=zero? '(rational) =zero?)

;; in install-complex-package
(define (=zero? x) (and (= (real-part x) 0) (= (imag-part x) 0)))
(put '=zero? '(complex) =zero?)

;; Typecasting
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc (apply proc (map contents args))
          (if (= length args 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type1 type2)))
                  (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                        (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types" (list op type-tags))))))
              (error "No method for these types" (list op type-tags)))))))

;; Exercise 2.81
;; a) Adding conversions for a type to itself will cause the program to loop
;; forever in the case that an operation is missing.
;;
;; b) If they already have the same type then coercing them to the same type
;; won't change anything.
;;
;; c) Modify apply-generic so it doesn't try to coerce types unnecessarily
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc (apply proc (map contents args))
          (if (= length args 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (equal? type1 type2)
                    (error "No method for these types" (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type1 type2)))
                      (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                            (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                            (else (error "No method for these types" (list op type-tags)))))))
              (error "No method for these types" (list op type-tags)))))))

;; Exercise 2.82
;; We want to generalise apply-generic to work with an arbitrary of arguments.
;;
;; One strategy is to try coercing all of the types to that of the first
;; argument, then the second, and so on. However, this will miss combinations
;; of types. For instance, this strategy won't try any types that aren't
;; included in the call.
;;
;; A better strategy would be to try find some "shared ancestor" type.
(define (apply-generic op . args)
  (define (no-method type-tags)
    (error "No method for these types" (list op type-tags))))

;; Exercise 2.83
(define (raise x) (apply-generic 'raise x))

;; int->rat: (make-rational x 1)
;; rat->real: (make-real (/ (numer x) (denom x)))
;; real->complex: (make-from-real-imag x 0)

;; Exercise 2.84
;; TODO: come back and finish this section off and build the symbolic
;; algebra example
