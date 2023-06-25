#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "m-eval/eval-apply.rkt")
(require "m-eval/global-environment.rkt")

; main
(eval 
  '
  (define 
    (a x)

    (define a x)
    (define b 5)
    (define c 4)
    (define 
      (d x)

      (define c x)
      (cons a (cons b (cons c '())))
    )
    (set! b 2)
    (d 3)
  )
  the-global-environment
)
; 'ok
(eval '(a 1) the-global-environment)
; (mcons 1 (mcons 2 (mcons 3 '())))
