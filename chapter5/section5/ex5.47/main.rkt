#lang racket
(provide (all-defined-out))

(require sicp)
(require "common/global-environment.rkt")
(require "machine/make-machine.rkt")
(require "machine/assembler/assemble.rkt")
(require "ec-eval/repl.rkt")
(require "ec-eval/eval-apply.rkt")
(require "compiler/compile.rkt")
(require "compiler/instruction-sequence.rkt")

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
    '(exp env val proc argl continue unev compapp)
    (append 
      (list 
        (list 'read read-inputs)
        (list 'prompt-for-input (lambda (_) '()))
      )
      repl-operations
      eval-operations
      apply-operations
      compile-operations
    )
    (append repl-insts eval-insts apply-insts)
  )
)
(define 
  (compile-and-go expression)
  (let 
    (
     ;
     (instructions 
       (assemble 
         (statements 
           (compile expression 'val 'return)
         )
         eceval
       )
     )
    )

    (setup-global-environment!)
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    (start eceval)
  )
)

; main
(set! 
  inputs
  '( ;
    (define 
      (compound-proc a b c)
      (display "this is compound proc, called by compiled proc, arguments: ")
      (displayln (list a b c))
      "return value"
    )
    (compiled-proc 1 2 3)
   )
)
(compile-and-go 
  '(define
    (compiled-proc a b c)
    (display "this is compiled proc, arguments: ")
    (displayln (list a b c))
    (compound-proc 4 5 6)
   )
)
