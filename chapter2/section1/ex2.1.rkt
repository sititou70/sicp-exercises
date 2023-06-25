#lang racket/base

(define 
  (make-rat n d)

  (if (> (* n d) 0) 
    (cons (abs n) (abs d))
    (cons (* (abs n) -1) (abs d))
  )
)
(define (numer x) (car x))
(define (denom x) (cdr x))

(define 
  (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
)

(print-rat (make-rat 1 2))
; 1/2
(print-rat (make-rat 3 -4))
; -3/4
(print-rat (make-rat -5 6))
; -5/6
(print-rat (make-rat -7 -8))
; 7/8
