#lang racket/base

(define (make-interval a b) (cons a b))
(define (lower-bound a) (car a))
(define (upper-bound a) (cdr a))

(define 
  (sub-interval x y)
  (make-interval 
    (- (lower-bound x) (upper-bound y))
    (- (upper-bound x) (lower-bound y))
  )
)

(sub-interval (make-interval 1 2) (make-interval 3 4))
; '(-3 -1)
(sub-interval (make-interval -1 2) (make-interval -3 4))
; '(-6 4)
(sub-interval (make-interval -2 -1) (make-interval -4 -3))
; '(1 3)
