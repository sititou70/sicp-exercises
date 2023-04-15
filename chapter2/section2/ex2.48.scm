#lang racket/base

(define 
  (make-vect x y)
  (cons x y)
)
(define 
  (xcor-vect v)
  (car v)
)
(define 
  (ycor-vect v)
  (cdr v)
)

(define 
  (make-segment start-vec end-vec)
  (cons start-vec end-vec)
)
(define 
  (start-segment seg)
  (car seg)
)
(define 
  (end-segment seg)
  (cdr seg)
)

(define 
  seg
  (make-segment 
    (make-vect 1 2)
    (make-vect 3 4)
  )
)
(start-segment seg)
; '(1 . 2)
(end-segment seg)
; '(3 . 4)
