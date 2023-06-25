#lang racket/base

(require "data-directed-utils.rkt")
(require "generic-procs.rkt")

; --------
; 強制型変換
; --------

(put-coercion 
  'scheme-number
  'rational
  (lambda (x) (make-rational (contents x) 1))
)
(put-coercion 
  'rational
  'complex
  (lambda (x) (make-complex-from-mag-ang (/ (numer x) (denom x)) 0))
)

(put-coercion 
  'rational
  'scheme-number
  (lambda (x) (make-scheme-number (/ (numer x) (denom x))))
)
(put-coercion 
  'complex
  'rational
  (lambda (x) (make-rational (real-part x) 1))
)

(put-supertype 'scheme-number 'rational)
(put-supertype 'rational 'complex)
