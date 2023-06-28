#lang racket
(provide (all-defined-out))

(require sicp)
(require "assembler/basic-machine-model.rkt")
(require "assembler/assemble.rkt")
(require "assembler/data-structures/register.rkt")

(define 
  (make-machine ops controller-text)
  (let 
    ( ;
     (machine (make-new-machine))
    )

    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence) 
      (assemble controller-text machine)
    )
    machine
  )
)
(define 
  (start machine)
  (machine 'start)
)
(define 
  (get-register-contents machine register-name)
  (get-contents (get-register machine register-name))
)
(define 
  (set-register-contents! machine register-name value)
  (set-contents! 
    (get-register machine register-name)
    value
  )
  'done
)
