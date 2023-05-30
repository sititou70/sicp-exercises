#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "repl.scm")
(require "eval-apply.scm")

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
