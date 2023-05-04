#lang racket/base

(require "data-directed-utils.scm")
(require "generic-procs.scm")

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
