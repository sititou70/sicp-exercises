#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "eval-apply.scm")
(require "global-environment.scm")

; main
(eval 
  '(let
    ( ;
     (a 1)
     (b (+ 1 1))
    )

    (displayln "hello")
    (+ (* b b) a)
   )
  the-global-environment
)
; hello
; 5
