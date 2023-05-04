#lang racket/base

(require "generic-procs.scm")
(require "scheme-number.scm")
(require "rational.scm")
(require "complex.scm")
(require "polynomial.scm")
(require "coercion.scm")

(define 
  p1
  (make-polynomial 
    'x
    '((4 1) (3 -1) (2 -2) (1 2))
  )
)
(define p2 (make-polynomial 'x '((3 1) (1 -1))))
(greatest-common-divisor p1 p2)
