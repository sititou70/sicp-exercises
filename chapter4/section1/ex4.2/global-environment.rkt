#lang racket
(provide (all-defined-out))

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "eval-apply.rkt")
(require "internal-data-structure.rkt")
(require "primitive-procedures.rkt")

; global environment
(define 
  (setup-environment)
  (let 
    ( ;
     (initial-env 
       (extend-environment 
         (primitive-procedure-names)
         (primitive-procedure-objects)
         the-empty-environment
       )
     )
    )

    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env
  )
)
(define the-global-environment (setup-environment))
