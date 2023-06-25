#lang racket/base

(define (make-interval a b) (cons a b))
(define (lower-bound a) (car a))
(define (upper-bound a) (cdr a))

(define 
  (mul-interval x y)
  (let 
    ((p1 (* (lower-bound x) (lower-bound y))) 
      (p2 (* (lower-bound x) (upper-bound y)))
      (p3 (* (upper-bound x) (lower-bound y)))
      (p4 (* (upper-bound x) (upper-bound y)))
    )
    (make-interval 
      (min p1 p2 p3 p4)
      (max p1 p2 p3 p4)
    )
  )
)
(define 
  (div-interval x y)

  (if (< (* (lower-bound y) (upper-bound y)) 0) 
    (error "interval y contains zero.")
    (mul-interval 
      x
      (make-interval 
        (/ 1.0 (upper-bound y))
        (/ 1.0 (lower-bound y))
      )
    )
  )
)

(div-interval (make-interval 1 2) (make-interval 1 2))
; '(0.5 2.0)

(div-interval (make-interval 1 2) (make-interval -1 2))
; error
