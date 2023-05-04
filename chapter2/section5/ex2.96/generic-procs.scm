#lang racket/base
(provide (all-defined-out))

(require "data-directed-utils.scm")

; --------
; ジェネリック演算
; --------

(define 
  (make-rational n d)
  ((get 'make 'rational) n d)
)
(define 
  (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y)
)
(define 
  (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a)
)
(define 
  (make-polynomial var terms)
  ((get 'make 'polynomial) var terms)
)

(define 
  (add x y)
  (if (and (number? x) (number? y)) 
    (+ x y)
    (apply-generic 'add x y)
  )
)
(define 
  (sub x y)
  (if (and (number? x) (number? y)) 
    (- x y)
    (apply-generic 'sub x y)
  )
)
(define 
  (mul x y)
  (if (and (number? x) (number? y)) 
    (* x y)
    (apply-generic 'mul x y)
  )
)
(define 
  (div x y)
  (if (and (number? x) (number? y)) 
    (/ x y)
    (apply-generic 'div x y)
  )
)

(define (numer x) (apply-generic 'numer x))
(define (denom x) (apply-generic 'denom x))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define 
  (equ? x y)
  (if (and (number? x) (number? y)) 
    (= x y)
    (apply-generic 'equ? x y)
  )
)

(define 
  (=zero? x)
  (if (number? x) 
    (= x 0)
    (apply-generic '=zero? x)
  )
)

(define 
  (additive-inverse x)
  (if (number? x) 
    (* x -1)
    (apply-generic 'additive-inverse x)
  )
)

(define 
  (greatest-common-divisor x y)
  (if (and (number? x) (number? y)) 
    (gcd x y)
    (apply-generic 'greatest-common-divisor x y)
  )
)
