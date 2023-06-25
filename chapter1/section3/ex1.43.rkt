#lang racket/base

(define 
  (inc x)
  (+ x 1)
)

(define 
  (square x)
  (* x x)
)

(define 
  (compose f g)
  (lambda (x) (f (g x)))
)

(define 
  (repeated f n)

  (cond 
    ((= n 1) f)
    ((> n 1) (compose f (repeated f (- n 1))))
    (else (error "Argument n must be positive and non-zero value."))
  )
)

((repeated square 2) 5)
; 625
