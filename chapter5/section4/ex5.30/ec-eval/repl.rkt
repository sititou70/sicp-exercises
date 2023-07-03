#lang racket
(provide (all-defined-out))

(require sicp)
(require "global-environment.rkt")

(define 
  repl-insts
  '( ;
    read-eval-print-loop
    (perform (op initialize-stack))
    (perform 
      (op prompt-for-input)
      (const "[EC-Eval] > ")
    )
    (assign exp (op read))
    (assign env (op get-global-environment))
    (assign continue (label print-result))
    (goto (label eval-dispatch))

    print-result
    (perform (op user-print) (reg val))
    (goto (label read-eval-print-loop))

    signal-val-error
    (perform (op user-print) (reg val))
    (goto (label read-eval-print-loop))
    signal-exp-error
    (perform (op user-print) (reg exp))
    (goto (label read-eval-print-loop))
    signal-env-error
    (perform (op user-print) (reg env))
    (goto (label read-eval-print-loop))
   )
)

(define 
  repl-operations
  (list 
    (list 'prompt-for-input display)
    (list 'user-print displayln)
    (list 'read read)
    (list 'get-global-environment (lambda () the-global-environment))
  )
)
