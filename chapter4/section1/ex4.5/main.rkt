#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "eval-apply.rkt")
(require "global-environment.rkt")

; main
(eval 
  '(cond 
     ((assoc 'b '((a 1) (b 2))) => cadr)
     (else false)
   )
  the-global-environment
)
; 2

(eval '(define (id x) x) the-global-environment)
; 'ok
(eval 
  '(define
    (t)
    (displayln "t evaluated")
    true
   )
  the-global-environment
)
; 'ok
(eval 
  '(cond 
     ((t) => id)
     (else false)
   )
  the-global-environment
)
; t evaluated
; #t