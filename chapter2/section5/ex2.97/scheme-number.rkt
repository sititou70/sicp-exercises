#lang racket/base

(require "data-directed-utils.rkt")

; --------
; 通常の算術演算パッケージ
; --------

(define 
  (install-scheme-number-package)

  (put 
    'add
    '(scheme-number scheme-number)
    (lambda (x y) (+ x y))
  )
  (put 
    'sub
    '(scheme-number scheme-number)
    (lambda (x y) (- x y))
  )
  (put 
    'mul
    '(scheme-number scheme-number)
    (lambda (x y) (* x y))
  )
  (put 
    'div
    '(scheme-number scheme-number)
    (lambda (x y) (/ x y))
  )
  (put 
    'equ?
    '(scheme-number scheme-number)
    (lambda (x y) (= x y))
  )
  (put 
    '=zero?
    '(scheme-number)
    (lambda (x) (= x 0))
  )
  (put 
    'additive-inverse
    '(scheme-number)
    (lambda (x) (* x -1))
  )
  (put 
    'reduce
    '(scheme-number scheme-number)
    (lambda (x y) 
      (let 
        ((g (gcd x y)))
        (list (/ x g) (/ y g))
      )
    )
  )
  (put 'make 'scheme-number (lambda (x) x))

  'done
)
(install-scheme-number-package)
