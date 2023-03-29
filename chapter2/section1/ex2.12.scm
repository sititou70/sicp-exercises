#lang racket/base

(define (make-interval a b) (cons a b))
(define (lower-bound a) (car a))
(define (upper-bound a) (cdr a))

(define 
  (make-center-percent c p)
  (make-interval (- c (* c (/ p 100))) (+ c (* c (/ p 100))))
)
(define 
  (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2)
)
(define 
  (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2)
)
(define 
  (percent i)
  (* (/ (width i) (center i)) 100)
)

(make-center-percent 6.8 10)
; '(6.12 . 7.4799999999999995)
(make-center-percent 4.7 5)
; '(4.465 . 4.9350000000000005)

(percent (make-center-percent 6.8 10))
; 9.999999999999996
(percent (make-center-percent 4.7 5))
; 5.000000000000006

