#lang racket/base

(require "generic-procs.rkt")
(require "scheme-number.rkt")
(require "rational.rkt")
(require "complex.rkt")
(require "polynomial.rkt")
(require "coercion.rkt")

(define 
  p1
  (make-polynomial 
    'x
    '((4 1) (3 -1) (2 -2) (1 2))
  )
)
(define p2 (make-polynomial 'x '((3 1) (1 -1))))
(greatest-common-divisor p1 p2)
