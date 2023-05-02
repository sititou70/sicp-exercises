#lang racket/base

(require "generic-procs.scm")
(require "scheme-number.scm")
(require "rational.scm")
(require "complex.scm")
(require "polynomial.scm")
(require "coercion.scm")

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
