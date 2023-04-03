#lang racket/base

(define 
  (square x)
  (* x x)
)

(define (divides? a b) (= (remainder b a) 0))
(define 
  (find-divisor n test-divisor)
  (cond 
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))
  )
)
(define (smallest-divisor n) (find-divisor n 2))
(define 
  (prime? n)
  (= n (smallest-divisor n))
)

(define 
  (enumerate-interval low high)
  (if (> low high) 
    null
    (cons low (enumerate-interval (+ low 1) high))
  )
)

(define 
  (accumulate op initial sequence)
  (if (null? sequence) 
    initial
    (op 
      (car sequence)
      (accumulate op initial (cdr sequence))
    )
  )
)

(define 
  (flatmap proc seq)
  (accumulate append null (map proc seq))
)

(define 
  (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair)))
)

(define 
  (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair)))
)

(define 
  (unique-pairs n)
  (flatmap 
    (lambda (i) 
      (map 
        (lambda (j) (list i j))
        (enumerate-interval 1 (- i 1))
      )
    )
    (enumerate-interval 1 n)
  )
)
(unique-pairs 6)
; '((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4) (6 1) (6 2) (6 3) (6 4) (6 5))

(define 
  (prime-sum-pairs n)
  (map 
    make-pair-sum
    (filter 
      prime-sum?
      (unique-pairs n)
    )
  )
)

(prime-sum-pairs 6)
; '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))
