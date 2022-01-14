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
  (cond ((null? items) #nil)
        ((null? (cdr items)) items)
        (else (safe-last-pair (cdr items)))))


;; Exercise 2.18
(define (iter-reverse-list items)
  (define (iter items result)
    (if (null? items) #nil
        (iter (cdr items)
              (cons (car items) result))))
  (iter items #nil))

(define (reverse-list items)
  (if (null? items) #nil
      (append (reverse-list (cdr items))
              (list (car items)))))
;; Use append because we can't cons backwards

(reverse-list (list 1 4 9 16 25))
(reverse-list #nil)

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
      (if (null? items) #nil
          (if (pred (car items))
              (cons (car items) (loop (cdr items)))
              (loop (cdr items)))))
    (loop xs)))

;; Therefore, a basic filter function is just:
(define (basic-filter pred items)
  (cond ((null? items) #nil)
        ((pred (car items))
         (cons (car items) (basic-filter pred (cdr items))))
        (else (basic-filter pred (cdr items)))))

;; Exercise 2.21
(define (square x) (* x x))

(define (square-list items)
  (map square items))

;; Without using map
(define (square-list items)
  (if (null? items) #nil
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
  (iter items #nil))
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
  (iter items #nil))

;; Another solution
(define (iter-square-list items)
  (define (iter xs pick)
    (define r (square (car xs)))
    (if (null? (cdr xs)) (pick (list r))
        (iter (cdr xs) (lambda (x) (pick (cons r x))))))
  (iter items (lambda (x) x)))
;; Consider (iter-square-list (list 1 2 3))
;;  (lambda (x) x)
;;  (lambda (x) (lambda (x) (cons 1 x)))
;;  (lambda (x) (lambda (x) (lambda (x) (cons 1 x))) (cons 4 x))
;;  (lambda (x) ((lambda (x) (lambda (x) (cons 1 x))) (cons 4 x)) (cons 9 nil))
;;  (lambda (x) (lambda (x) (cons 1 x)) (cons 4 (cons 9 nil)))
;;  (cons 1 (cons 4 (cons 9 nil)))


;; Exercise 2.23
(define (for-each proc items)
  (cond ((null? items) #t)              ; can return arbitrary value
        (else                           ; implicit `begin`
         (proc (car items))
         (for-each proc (cdr items)))))
;; Under the hood, can think of begin as creating a lambda and immediately
;; applying it.
;; We are introduced to the begin special form in Chapter 3

;; We can avoid multiple expressions by using another function
;; By the rules of function evaluation, val will be evaluated at each step
(define (for-each proc items)
  (define (iter items val)
    (if (null? items) #t
        (iter (cdr items) (proc (car items)))))
  (iter items #t))

;; Tree operations
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

;; Exercise 2.24
(list 1 (list 2 (list 3 4)))
;; => (1 (2 (3 4)))
;; As a tree
;;       (1 (2 (3 4))
;;       /         \
;;     1        (2 (3 4))
;;              /        \
;;             2        (3 4)
;;                      /   \
;;                      3    4

;; Exercise 2.25
(car (cdaddr (list 1 3 (list 5 7) 9)))
(car (car (list (list 7))))
(cadadr
 (cadadr
  (cadadr
   (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))

;; Exercise 2.26
;; x: (1 2 3), y: (4 5 6)
;;  append: (1 2 3 4 5 6)
;;  cons:   ((1 2 3) 4 5 6)    -- can think of as 'prepend atom'
;;  list:   ((1 2 3) (4 5 6))  -- makes a list of the items

;; Exercise 2.27
(define (iter-deep-reverse items)
  (define (iter items ans)
    (cond ((null? items) ans)
          ((not (pair? items)) items)
          (else
           (iter (cdr items) (cons (deep-reverse (car items)) ans)))))
  (iter items #nil))

(define (deep-reverse items)
  (if (pair? items)
      (append (deep-reverse (cdr items))
              (list (deep-reverse (car items))))
      items))

;; Exercise 2.28
(define (fringe tree)
  (cond ((null? tree) #nil)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree)) (fringe (cdr tree))))))

(define (fringe tree)
  (define (step tree ans)
    (cond ((null? tree) ans)
          ((not (pair? tree)) (cons tree ans))
          (else (step (car tree) (step (cdr tree) ans)))))
  (step tree #nil))

;; Exercise 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

;; Selectors
;; These abstract away implementation details (just need to change these
;; if we change implementation)
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length branch) (car branch))
(define (branch-struc branch) (cadr branch))
(define (mobile? mobile) (pair? mobile))

(define (total-weight mobile)
  (if (not (mobile? mobile)) mobile
      (+ (total-weight (branch-struc (left-branch mobile)))
         (total-weight (branch-struc (right-branch mobile))))))

(define a (make-mobile (make-branch 2 3) (make-branch 2 3)))
(total-weight a)
;; => 6
(define b (make-mobile
           (make-branch 3 (make-mobile (make-branch 2 3) (make-branch 2 3)))
           (make-branch 3 4)))
(total-weight b)
;; => 10

(define (balanced? mobile)
  (define (torque branch)
    (* (branch-length branch) (total-weight (branch-struc branch))))
  (if (not (mobile? mobile)) #t
      (let ((left (left-branch mobile))
            (right (right-branch mobile)))
        (and (= (torque left) (torque right))
             (balanced? (branch-struc left))
             (balanced? (branch-struc right))))))

;; Without using total-weight (avoid unnecessary traversals)
(define (balanced? mobile)
  (define (recur-pair mobile)
    ;; Store traversal as pair (balanced, weight)
    (if (not (mobile? mobile)) (cons #t mobile)
        (let ((left  (recur-pair (branch-struc (left-branch  mobile))))
              (right (recur-pair (branch-struc (right-branch mobile)))))
          (cons (and (car left)
                     (car right)
                     (= (* (branch-length (left-branch mobile)) (cdr left))
                        (* (branch-length (right-branch mobile)) (cdr right))))
                (+ (cdr left) (cdr right))))))
  (car (recur-pair mobile)))

(balanced? a)
;; => #t
(balanced? b)
;; => #f

;; Exercise 2.30
(define (square-tree tree)
  (map (lambda (subtree)
         (if (pair? subtree) (square-tree subtree)
             (* subtree subtree)))
       tree))

(define (square-tree tree)
  (cond ((null? tree) #nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

;; Exercise 2.31
(define (tree-map proc tree)
  (cond ((null? tree) #nil)
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))

;; Exercise 2.32
(define (subsets s)
  (if (null? s) (list #nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;; Sequence operations
(define (toby/filter pred seq)
  (cond ((null? seq) #nil)
        ((pred (car seq))
         (cons (car seq)
               (toby/filter pred (cdr seq))))
        (else (toby/filter pred (cdr seq)))))

(define (accumulate op init seq)
  (if (null? seq) init
      (op (car seq)
          (accumulate op init (cdr seq)))))

;; Exercise 2.33
;; We can define basic list operations in terms of accumulate
(define (toby/map proc seq)
  (accumulate
   (lambda (x y)
     (cons (proc x) y))
   #nil
   seq))
(define (toby/append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (toby/length seq)
  (accumulate (lambda (_ y) (+ 1 y)) 0 seq))

;; Exercise 2.34
;; Assume coefficients are a_0, ..., a_n
(define (horner-eval x coeffs)
  (accumulate (lambda (coeff rest) (+ coeff (* rest x))) 0 coeffs))

;; Exercise 2.35
(define (count-leaves tree)
  (accumulate + 0 (map
                   (lambda (x)
                     (cond ((null? x) 0)
                           ((pair? x) (count-leaves x))
                           (else 1)))
                   tree)))
;; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs)) #nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 s)
;; => (22 26 30)

;; Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose m)
  (accumulate-n cons #nil m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r) (matrix-*-vector cols r)) m)))

(define I (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(define A (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(matrix-*-matrix A I)
;; => ((1 2 3) (4 5 6) (7 8 9))

;; Exercise 2.38
(define fold-right accumulate)
(define (fold-left op init seq)
  (define (iter ans rest)
    (if (null? rest) ans
        (iter (op ans (car rest))
              (cdr rest))))
  (iter init seq))

(fold-right / 1 (list 1 2 3))           ; nest right
;; => 3/2
(fold-left  / 1 (list 1 2 3))           ; nest left
;; => 1/6
(fold-right list #nil (list 1 2 3))
;; => (1 (2 (3 #nil)))
(fold-left  list #nil (list 1 2 3))
;; => (((#nil 1) 2) 3)

(define (flip-cons rest first)
  (cons first rest))

(fold-right cons #nil '(1 2 3))         ; identity fn
(fold-left flip-cons #nil '(1 2 3))     ; reverse

;; The only difference between fold-left and fold-right is the nesting of
;; brackets. Thus, given an associative operation, fold-left and fold-right will
;; produce the same result.

;; Exercise 2.39
(define (reverse-left seq)
  (fold-left (lambda (x y) (cons y x)) #nil seq))
(define (reverse-right seq)
  (fold-right (lambda (x y) (append y (list x))) #nil seq))

;; Nested mapping
(define (flatmap proc seq)
  (accumulate append #nil (map proc seq)))

;; Exercise 2.40
(load "../library.scm")

(define (unique-pairs n)
  ;; Generates (i,j,k) where 1 <= j < i <= n
  (flatmap (lambda (i)
             (map (lambda (j) (list j i))
                  (range 1 (1- i))))
           (range 2 n)))

(define (prime-sum? pair)
  ;; Here a pair means (list x y) instead of a Lisp pair
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

;; Exercise 2.41
(define (unique-triples n)
  ;; Generates (i,j,k) where 1 <= k < j < i <= n
  (flatmap
   (lambda (i)
     (flatmap (lambda (j)
                (map (lambda (k)
                       (list k j i))
                     (range 1 (1- j))))
          (range 2 (1- i))))
   (range 3 n)))

(define (ordered-triple-sum n s)
  (filter (lambda (t) (= (sum t) s)) (unique-triples n)))

(define (fast-three-sum n s)            ; O(n^2)
  (define (valid? triple)
    (let ((i (car triple))
          (j (cadr triple))
          (k (caddr triple)))
      (< 0 i j k)))
  (define triples
    (flatmap
     (lambda (i) (map (lambda (j) (list (- s j i) j i)) (range 2 (1- i))))
     (range 3 n)))
  (filter valid? triples))

;; Exercise 2.42
;; n-queens problem
(define (queens board-size)
  (define empty-board #nil)

  (define (attacked? k seq)
    (let ((new-queen (caar seq))
          (queens (cdr seq)))
      (accumulate
       (lambda (queen rest)
         (or (= new-queen (car queen))
             (= new-queen (+ (car queen) (- (cadr queen) k)))
             (= new-queen (- (car queen) (- (cadr queen) k)))
             rest))
       #f
       queens)))

  (define (adjoin-position new-queen k queens)
    ;; Add possibilities to kth col
    (cons (list new-queen k) queens))

  (define (queen-cols k)
    (if (= k 0) (list empty-board)
        (filter
         (lambda (positions) (not (attacked? k positions)))
         (flatmap
          (lambda (queens)
            (map (lambda (new-queen)
                   (adjoin-position new-queen k queens))
                 (range 1 board-size)))
          (queen-cols (1- k))))))
  (queen-cols board-size))

;; Exercise 2.43
;;
;; The most time-consuming part of the queens program is queens-col. For the
;; original program, it's called N+1 times (although (queens-col 0) returns
;; instantly). In Louis's program, he's wrapped the call to queen-cols in a loop
;; so each call results in N further calls to queen-cols.
;;
;; The recursion in Louis's program will result in a tree N nodes deep where
;; each node has N children. Thus, the number of nodes in the tree will be
;; O(N^N). The difference between that and the original program is O(N^N) / N+1
;; = O(N^N) so Louis's program will be roughly O(N^N) times slower.
