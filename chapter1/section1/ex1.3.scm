#lang racket/base

(define 
  (square x)
  (* x x)
)

(define 
  (sum-of-squares x y)
  (+ (square x) (square y))
)

(define 
  (maximum-sum-of-squares x y z)
  (cond 
    ((and (<= x y) (<= x z)) (sum-of-squares y z))
    ((and (<= y z) (<= y z)) (sum-of-squares x z))
    ((and (<= z x) (<= z y)) (sum-of-squares x y))
  )
)

(maximum-sum-of-squares 1 2 3)
; 13

(maximum-sum-of-squares 3 1 2)
; 13

(maximum-sum-of-squares 7 7 7)
; 98
