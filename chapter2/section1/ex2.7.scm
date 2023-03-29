#lang racket/base

(define (make-interval a b) (cons a b))
(define (lower-bound a) (car a))
(define (upper-bound a) (cdr a))

(define 
  (add-interval x y)
  (make-interval 
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))
  )
)
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
  (mul-interval 
    x
    (make-interval 
      (/ 1.0 (upper-bound y))
      (/ 1.0 (lower-bound y))
    )
  )
)

(define 
  (func r1 r2)
  (let 
    ((one (make-interval 1 1)))
    (div-interval 
      one
      (add-interval 
        (div-interval one r1)
        (div-interval one r2)
      )
    )
  )
)

(func (make-interval 6.12 7.48) (make-interval 4.465 4.935))
; '(2.581558809636278 . 2.97332259363673)
