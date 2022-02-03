;; Useful functions
(define (square x) (* x x))

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (define (divides? a b)
      (= (remainder b a) 0))
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (1+ test-divisor)))))
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (range start end)
  (if (< end start) #nil
      (cons start (range (1+ start) end))))

(define (andmap pred seq)
  (cond ((null? seq) #t)
        ((pred (car seq)) (andmap pred (cdr seq)))
        (else #f)))

(define (ormap pred seq)
  (cond ((null? seq) #f)
        ((pred (car seq)) #t)
        (else (ormap pred (cdr seq)))))

(define (accumulate op init seq)
  (if (null? seq) init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append #nil (map proc seq)))

(define (sum items)
  (accumulate + 0 items))

(define (unique-tuples n k)
  ;; Generates k-tuples where all elements are distinct and <= n
  (define (iter m k)
    (if (= k 0) (list #nil)
        (flatmap
         (lambda (i)
           (map (lambda (t) (cons i t))
                (iter (1+ i) (1- k))))
         (range m n))))
  (iter 1 k))
