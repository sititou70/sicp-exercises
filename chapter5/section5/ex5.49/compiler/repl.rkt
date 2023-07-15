#lang racket
(provide (all-defined-out))

(require sicp)
(require "compile.rkt")
(require "instruction-sequence.rkt")
(require "global-environment.rkt")

(define 
  repl-texts
  '( ;
    read-eval-print-loop
    (perform (op initialize-stack))
    (perform 
      (op prompt-for-input)
      (const "[EC-Eval] > ")
    )
    (assign env (op get-global-environment))
    (assign continue (label print-result))
    (assign val (op read))
    (assign val (op compile) (reg val))
    (assign val (op assemble) (reg val)) ; assembleはmain.rktで定義する。マシンインスタンスが必要なため
    (goto (reg val))

    print-result
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
    (list 
      'compile
      (lambda (exp) 
        (statements 
          (compile exp 'val 'return)
        )
      )
    )
  )
)
