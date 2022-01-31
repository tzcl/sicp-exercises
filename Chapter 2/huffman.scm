(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left tree) (car tree))
(define (right tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree) (list (symbol-leaf tree)) (caddr tree)))
(define (weight tree)
  (if (leaf? tree) (weight-leaf tree) (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits curr)
    (if (null? bits) '()
        (let ((next (choose-branch (car bits) curr)))
          (if (leaf? next)
              (cons (symbol-leaf next)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next)))))
  (decode-1 bits tree))
;; The stack depth of this function is proportional to the input size.
;; It causes stack overflows for inputs of 1000+ bits

(define (decode bits tree)
  (define (iter bits branch message)
    (if (null? bits) message
        (let ((next (choose-branch (car bits) branch)))
          (if (leaf? next)
              (iter (cdr bits) tree (cons (symbol-leaf next) message))
              (iter (cdr bits) next message)))))
  (reverse (iter bits tree '())))
;; A common pattern in list processing is to cons items since cons is
;; O(1) then reverse once (appending each item is O(n^2)).
;; Lists are often built up backwards

(define (choose-branch bit branch)
  (cond ((= bit 0) (left branch))
        ((= bit 1) (right branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs) '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;; Exercise 2.67
(define sample-tree
  (make-code-tree
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree (make-leaf 'D 1) (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)
;; => (A D A B B C A)

;; Exercise 2.68
(define (encode message tree)
  (if (null? message) '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((memq symbol (symbols (left tree)))
         (cons 0 (encode-symbol symbol (left tree))))
        ((memq symbol (symbols (right tree)))
         (cons 1 (encode-symbol symbol (right tree))))
        (else (error "Invalid symbol:" symbol))))

(define (encode-symbol symbol tree)
  (define (iter branch encoding)
    (cond ((leaf? branch) encoding)
          ((memq symbol (symbols (left branch)))
           (iter (left branch) (cons 0 encoding)))
          ((memq symbol (symbols (right branch)))
           (iter (right branch) (cons 1 encoding)))
          (else (error "Invalid symbol:" symbol))))
  (reverse (iter tree '())))

(equal? (encode (decode sample-message sample-tree) sample-tree) sample-message)
;; => #t

;; Exercise 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaves)
  (if (null? (cdr leaves)) (car leaves)
      (successive-merge
       (adjoin-set (make-code-tree (car leaves) (cadr leaves))
                   (cddr leaves)))))

(equal? (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
        sample-tree)
;; => #t

;; Exercise 2.70
(define rock-alphabet
  '((a 2) (get 2) (sha 3) (wah 1)
    (boom 1) (job 2) (na 16) (yip 9)))
(define rock-tree (generate-huffman-tree rock-alphabet))
(define rock-song '(get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip yip sha boom))
(define encoded-rock-song (encode rock-song rock-tree))

;; Difference in bits
(length encoded-rock-song)
;; => 84
(* 3 (length rock-song))
;; => 108
