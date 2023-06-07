#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "m-eval/eval-apply.scm")
(require "m-eval/global-environment.scm")

(define a-val actual-value)

; main
; cons car cdr
(a-val 
  '(define (cons x y) (lambda (m) (m x y)))
  the-global-environment
)
; 'ok
(a-val 
  '(define (car z) (z (lambda (p q) p)))
  the-global-environment
)
; 'ok
(a-val 
  '(define (cdr z) (z (lambda (p q) q)))
  the-global-environment
)
; 'ok

; list utils
(a-val '(car '(a b c)) the-global-environment)
; 'a
(a-val '(car (cdr '(a b c))) the-global-environment)
; 'b
(a-val '(car (cdr (cdr '(a b c)))) the-global-environment)
; 'c
(a-val '(cdr (cdr (cdr '(a b c)))) the-global-environment)
; '()
