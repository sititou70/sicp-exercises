#lang racket
(provide (all-defined-out))

(require sicp)
(require "environment.scm")
(require "procedure.scm")

; global environment
(define 
  (setup-environment)
  (let 
    ( ;
     (initial-env 
       (extend-environment 
         primitive-procedures
         the-empty-environment
       )
     )
    )

    (define-variable! (cons 'true true) initial-env)
    (define-variable! (cons 'false false) initial-env)
    initial-env
  )
)
(define the-global-environment (setup-environment))
