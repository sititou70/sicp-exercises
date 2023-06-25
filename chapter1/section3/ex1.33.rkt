#lang racket/base

; prime
(define 
  (square x)
  (* x x)
)
(define (smallest-divisor n) (find-divisor n 2))
(define 
  (find-divisor n test-divisor)
  (cond 
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))
  )
)
(define (divides? a b) (= (remainder b a) 0))
(define 
  (prime? n)
  (= n (smallest-divisor n))
)

; gcd
(define 
  (gcd a b)
  (if (= b 0) 
    a
    (gcd 
      b
      (remainder a b)
    )
  )
)

(define 
  (filtered-accumulate combiner null-value term a next b filter?)

  (define 
    (iter a result)
    (if (> a b) 
      result
      (iter 
        (next a)
        (combiner 
          result
          (if (filter? a) 
            (term a)
            null-value
          )
        )
      )
    )
  )

  (iter a null-value)
)

; a
(define 
  (sum-squared-prime a b)

  (define (inc n) (+ n 1))

  (filtered-accumulate + 0 square a inc b prime?)
)
(sum-squared-prime 2 100)

; b
(define 
  (prod-disjoints n)

  (define (identity n) n)
  (define (inc n) (+ n 1))

  (define (filter? i) (= (gcd i n) 1))

  (filtered-accumulate * 1 identity 1 inc n filter?)
)
(prod-disjoints 12)
