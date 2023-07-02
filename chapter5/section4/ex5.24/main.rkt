#lang racket
(provide (all-defined-out))

(require sicp)
(require "machine/make-machine.rkt")
(require "ec-eval/repl.rkt")
(require "ec-eval/eval-apply.rkt")

; main
(define inputs '())
(define 
  (read-inputs)
  (if (null? inputs) 
    (begin (displayln "no more input") (exit))
  )
  (let 
    ( ;
     (input (car inputs))
    )

    (set! inputs (cdr inputs))
    input
  )
)
(define 
  eceval
  (make-machine 
    '(exp env val proc argl continue unev)
    (append 
      (list 
        (list 'read read-inputs)
        (list 'prompt-for-input (lambda (_) '()))
      )
      repl-operations
      eval-operations
      apply-operations
    )
    (append repl-insts eval-insts apply-insts)
  )
)

(set! 
  inputs
  '
  ( ;
   (define a 1)
   ; ok

   (cond)
   ; false

   (cond 
     (false 999)
   )
   ; false

   (cond 
     (true 999)
   )
   ; 999

   (cond 
     (else 123)
   )
   ; 123

   (cond 
     (false 999)
     ((= a 1) 1)
   )
   ; 1

   (cond 
     (false 999)
     (else 2)
   )
   ; 2

   (cond 
     ((cond 
        (false 1)
        (true true)
      )
      123
     )
   )
   ; 123

   (cond 
     (true
      (set! a 2)
      a
     )
   )
   ; 2

   (cond 
     (else 1)
     (true 2)
   )
   ; ELSE clause isn't last
  )
)

(start eceval)
