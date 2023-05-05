#lang racket/base

(define 
  (make-accumulator initial)
  (define count initial)

  (lambda (val) 
    (set! count (+ count val))
    count
  )
)

(define A (make-accumulator 5))

(A 10)
; 15

(A 10)
; 25
