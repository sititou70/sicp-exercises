#lang racket/base

(require "generic-procs.rkt")
(require "scheme-number.rkt")
(require "rational.rkt")
(require "complex.rkt")
(require "polynomial.rkt")
(require "coercion.rkt")

(div 
  (make-polynomial 
    'x
    (list 
      (list 5 (make-scheme-number 1))
      (list 0 (make-scheme-number -1))
    )
  )
  (make-polynomial 
    'x
    (list 
      (list 2 (make-scheme-number 1))
      (list 0 (make-scheme-number -1))
    )
  )
)
