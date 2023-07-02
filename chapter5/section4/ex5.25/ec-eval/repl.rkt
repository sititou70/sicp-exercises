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
    (goto (label actual-value))

    print-result
    (perform (op user-print) (reg val))
    (goto (label read-eval-print-loop))

    unknown-expression-type
    (assign val (const unknown-expression-type-error))
    (goto (label signal-error))
    unknown-procedure-type
    (restore continue) ; (apply-dispatchの)スタックをクリアする
    (assign val (const unknown-procedure-type-error))
    (goto (label signal-error))
    signal-error
    (perform (op user-print) (reg val))
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
