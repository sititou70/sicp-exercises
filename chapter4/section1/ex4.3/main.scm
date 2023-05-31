#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "eval-apply.scm")
(require "global-environment.scm")

; main
(eval 
  '(define
    (factorial x)
    (if (= x 0) 
      1
      (* x (factorial (- x 1)))
    )
   )
  the-global-environment
)
; 'ok

(eval '(factorial 7) the-global-environment)
; 5040
