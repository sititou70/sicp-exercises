#lang racket/base

(require "generic-procs.rkt")
(require "scheme-number.rkt")
(require "rational.rkt")
(require "complex.rkt")
(require "polynomial.rkt")
(require "coercion.rkt")

(add 
  (make-rational 1 2)
  (make-rational 1 4)
)
; '(rational 6 . 8)

(define 
  r1-numer
  (make-polynomial 
    'x
    (list 
      (list 1 (make-scheme-number 1))
      (list 0 (make-scheme-number 1))
    )
  )
)
(define 
  r1-denom
  (make-polynomial 
    'x
    (list 
      (list 3 (make-scheme-number 1))
      (list 0 (make-scheme-number -1))
    )
  )
)

(define 
  r2-numer
  (make-polynomial 
    'x
    (list 
      (list 1 (make-scheme-number 1))
    )
  )
)
(define 
  r2-denom
  (make-polynomial 
    'x
    (list 
      (list 2 (make-scheme-number 1))
      (list 0 (make-scheme-number -1))
    )
  )
)
(add 
  (make-rational r1-numer r1-denom)
  (make-rational r2-numer r2-denom)
)
; '(rational 
;   (polynomial 
;     x
;     (4 (scheme-number . 1))
;     (3 (scheme-number . 1))
;     (2 (scheme-number . 1))
;     (1 (scheme-number . -2))
;     (0 (scheme-number . -1))
;   )
;   polynomial
;   x
;   (5 (scheme-number . 1))
;   (3 (scheme-number . -1))
;   (2 (scheme-number . -1))
;   (0 (scheme-number . 1))
; )

(define p1 (make-polynomial 'x '((2 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 1))))
(define rf (make-rational p2 p1))
(add rf rf)
; '(rational 
;   (polynomial x (5 2) (3 2) (2 2) (0 2))
;   polynomial  x (4 1) (2 2) (0 1)
; )
