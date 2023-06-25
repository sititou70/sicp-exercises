#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "m-eval/eval-apply.rkt")
(require "m-eval/global-environment.rkt")

; main
(eval 
  '((lambda (x) 
      (define 
        (f x)
        (let 
          ((a x))
          (cond 
            (false false)
            (else (set! a 2))
          )
          (if (= a 1) 
            'never
            (begin 
              (set! a 'allok)
              a
            )
          )
        )
      )
      (f x)
    )
    1
   )
  the-global-environment
)
; allok
