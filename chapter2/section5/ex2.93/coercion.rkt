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
  'complex
  'rational
  (lambda (x) (make-rational (real-part x) 1))
)
