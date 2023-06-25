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
  (add-vect v1 v2)
  (make-vect 
    (+ (xcor-vect v1) (xcor-vect v2))
    (+ (ycor-vect v1) (ycor-vect v2))
  )
)
(define 
  (sub-vect v1 v2)
  (make-vect 
    (- (xcor-vect v1) (xcor-vect v2))
    (- (ycor-vect v1) (ycor-vect v2))
  )
)
(define 
  (scale-vect v s)
  (make-vect 
    (* (xcor-vect v) s)
    (* (ycor-vect v) s)
  )
)

(add-vect (make-vect 1 2) (make-vect 3 4))
; '(4 . 6)
(sub-vect (make-vect 1 2) (make-vect 3 4))
; '(-2 . -2)
(scale-vect (make-vect 1 2) 3)
; '(3 . 6)
