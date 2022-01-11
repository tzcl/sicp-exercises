;; List functions
(define (list-ref items n)
  (if (= n 0) (car items)
      (list-ref (cdr items) (1- n))))

(define (length-list items)
  (if (null? items) 0
      (+ 1 (length (cdr items)))))

(define (iter-length items)
  (define (iter items n)
    (if (null? items) n
        (iter (cdr items) (1+ n))))
  (iter items 0))

(define (append list1 list2)
  (if (null? list1) list2
      (cons (car list1)
            (append (cdr list1) list2))))

;; Exercise 2.17
(define (last-pair items)               ; recursive, O(n)
  (if (null? (cdr items)) items
      (last-pair (cdr items))))

(define (fn-last-pair items)            ; also O(n)
  (list (list-ref items (1- (length items)))))

(last-pair (list 23 72 149 34))
;; => (34)

;; To make this work with empty lists, we can use cond instead
;; (avoid trying to do (cdr nil))
(define (safe-last-pair items)
  (cond ((null? items) '())
        ((null? (cdr items)) items)
        (else (safe-last-pair (cdr items)))))


;; Exercise 2.18
(define (iter-reverse-list items)
  (define (iter items result)
    (if (null? items) '()
        (iter (cdr items)
              (cons (car items) result))))
  (iter items '()))

(define (reverse-list items)
  (if (null? items) '()
      (append (reverse-list (cdr items))
              (list (car items)))))
;; Use append because we can't cons backwards

(reverse-list (list 1 4 9 16 25))
(reverse-list '())

;; Exercise 2.19
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(define (no-more? coins) (null? coins))
(define (except-first-denomination coins) (cdr coins))
(define (first-denomination coins) (car coins))

(define us-coins (list 50 25 10 5 1))
(define aus-coins (list 200 100 50 20 10 5))

(cc 100 us-coins)
(cc 100 (reverse us-coins))
(cc 100 aus-coins)
(cc 100 (reverse aus-coins))
;; It doesn't matter whether the lists are reversed or not

;; Exercise 2.20
(define (same-parity . xs)
  (let ((pred (if (even? (car xs)) even? odd?)))
    (filter pred xs)))

;; Without using filter
(define (same-parity . xs)
  (let ((pred (if (even? (car xs)) even? odd?)))
    (define (loop items)
      (if (null? items) '()
          (if (pred (car items))
              (cons (car items) (loop (cdr items)))
              (loop (cdr items)))))
    (loop xs)))

;; Therefore, a basic filter function is just:
(define (basic-filter pred items)
  (cond ((null? items) '())
        ((pred (car items))
         (cons (car items) (basic-filter pred (cdr items))))
        (else (basic-filter pred (cdr items)))))

;; Exercise 2.21
(define (square x) (* x x))

(define (square-list items)
  (map square items))

;; Without using map
(define (square-list items)
  (if (null? items) '()
      (cons (square (car items))
            (square-list (cdr items)))))

;; Exercise 2.22
;; Louis's first attempt at an iterative square-list
(define (iter-square-list items)
  (define (iter xs ans)
    (if (null? xs) ans
        (iter (cdr xs)
              (cons (square (car xs))
                    ans))))
  (iter items '()))
;; This gives the answer in reverse order since each iteration is taking the
;; previous answer and wrapping it in cons, i.e., prepending to ans
;; Consider (iter-square-list (list 1 2 3))
;;   ans: nil
;;   ans: (cons 1 nil)
;;   ans: (cons 4 (cons 1 nil))
;;   ans: (cons 9 (cons 4 (cons 1 nil))
;; This gives a valid list but in reverse order
;;
;; Louis's second attempt swaps the arguments to cons
;;
;; This won't work since you can't cons backwards! This is the problem I was
;; having trying to write an iterative version of reverse without extra memory.
;; Consider (iter-square-list (list 1 2 3))
;;   ans: nil
;;   ans: (cons nil 1)
;;   ans: ((cons nil 1) 2)
;;   ...
;; This is not a valid list
;;
;; One solution would be to use append instead of cons
(define (iter-square-list items)
  (define (iter xs ans)
    (if (null? xs) ans
        (iter (cdr xs)
              (append ans
                      (list (square (car xs)))))))
  (iter items '()))

;; Exercise 2.23
(define (for-each proc items)
  (cond ((null? items) #t)                  ; can return arbitrary value
        (else                               ; implicit `begin`
         (proc (car items))
         (for-each proc (cdr items)))))
;; Under the hood, can think of begin as creating a lambda and immediately
;; applying it.
;; We are introduced to the begin special form in Chapter 3
