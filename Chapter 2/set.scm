;; Using unordered lists
(define (set/elem? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (set/elem? x (cdr set)))))

(define (set/adjoin x set)
  (if (set/elem? x set) set (cons x set)))

(define (set/intersect set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((set/elem? (car set1) set2)
         (cons (car set1) (set/intersect (cdr set1) set2)))
        (else (set/intersect (cdr set1) set2))))

;; Exercise 2.59
(define (set/union set1 set2)
  (if (null? set1) set2
      (set/union (cdr set1) (set/adjoin (car set1) set2))))

;; Or using accumulate
(load "../library.scm")
(define (set/union set1 set2)
  (accumulate set/adjoin set2 set1))

;; Exercise 2.60
;; Assume we allow duplicates in our representation
(define (set/adjoin x set)
  (cons x set))

(define (set/union set1 set2)
  (append set1 set2))

;; As a result, checking if an item is in the set becomes more expensive,
;; e.g., consider (1 1 1 1 1 1 1 1 1 1 1 1 2).
;; However, the trade-off is that adjoining elements bceomes less expensive.
;;
;;                        No duplicates                     Duplicates
;; set/elem?              O(n)                              O(n + d)
;; set/adjoin             O(n)                              O(1)
;; set/intersect          O(n^2)                            O((n + d)^2)
;; set/union              O(n^2)                            O(n)
;;
;; Allowing duplicates, adjoining and union are faster so you would prefer
;; this representation if those operations are common.

;; Using ordered lists
;; Assume that we store set elements in increasing order
(define (set/elem? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)            ; we can skip the rest of the set
        (else (set/elem? x (cdr set)))))
;; This is still O(n) in the worst case but on average we expect to find
;; the element in the middle somewhere (so avg number of steps is n/2).

(define (set/intersect set1 set2)
  (if (or (null? set1) (null? set2)) '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2) (cons x1 (set/intersect (cdr set1) (cdr set2))))
              ((< x1 x2) (set/intersect (cdr set1) set2))
              ((< x2 x1) (set/intersect set1 (cdr set2)))))))
;; This is a speedup from O(n^2) to O(n)

;; Exercise 2.61
(define (set/adjoin x set)
  (if (set/elem? x set) set (cons x set)))

(define (set/adjoin x set)
  (cond ((null? set) (cons x set))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (set/adjoin x (cdr set))))))

;; Exercise 2.62
(define (set/union set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cons (min x1 x2)
                 (set/union (if (<= x1 x2) (cdr set1) set1)
                            (if (<= x2 x1) (cdr set2) set2)))))))

;; Using binary trees
(define (entry tree) (cadr tree))
(define (left tree) (car tree))
(define (right tree) (caddr tree))
(define (make-tree entry left right) (list left entry right))

(define (set/elem? x set)               ; O(log n)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set)) (set/elem? x (left set)))
        ((> x (entry set)) (set/elem? x (right set)))))

(define (set/adjoin x set)              ; O(log n) as long as tree is balanced
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) (set/adjoin x (left set)) (right set)))
        ((> x (entry set))
         (make-tree (entry set) (left set) (set/adjoin x (right set))))))
