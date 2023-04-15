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
  (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2)
)
(define 
  (origin-frame1 frame)
  (car frame)
)
(define 
  (edge1-frame1 frame)
  (cadr frame)
)
(define 
  (edge2-frame1 frame)
  (caddr frame)
)

(define 
  (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2))
)
(define 
  (origin-frame2 frame)
  (car frame)
)
(define 
  (edge1-frame2 frame)
  (cadr frame)
)
(define 
  (edge2-frame2 frame)
  (cddr frame)
)

(define 
  frame1
  (make-frame1 
    (make-vect 1 2)
    (make-vect 3 4)
    (make-vect 5 6)
  )
)
(origin-frame1 frame1)
; '(1 . 2)
(edge1-frame1 frame1)
; '(3 . 4)
(edge2-frame1 frame1)
; '(5 . 6)

(define 
  frame2
  (make-frame2 
    (make-vect 1 2)
    (make-vect 3 4)
    (make-vect 5 6)
  )
)
(origin-frame2 frame2)
; '(1 . 2)
(edge1-frame2 frame2)
; '(3 . 4)
(edge2-frame2 frame2)
; '(5 . 6)
