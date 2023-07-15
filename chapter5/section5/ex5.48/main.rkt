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
  (compile-and-assemble exp)
  (assemble 
    (statements 
      (compile exp 'val 'return)
    )
    eceval
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
        (list 'compile-and-assemble compile-and-assemble)
      )
      repl-operations
      eval-operations
      apply-operations
      compile-operations
    )
    (append repl-insts eval-insts apply-insts)
  )
)

; main
; コンパイル/アセンブルの結果に制御を移す処理は、本文中のexternal-entryがそのまま利用できる。
; external-entryへ制御を移すため、compile-and-runを基本手続きではなく特殊形式として組み込んだ。
; 参考：http://community.schemewiki.org/?sicp-ex-5.48
(set! 
  inputs
  '( ;
    (compile-and-run 
      '(define
        (factorial n)
        (if (= n 1) 1 (* (factorial (- n 1)) n))
       )
    )
    (factorial 5)

    (compile-and-run 
      '(define
        (compiled-proc1 a b c)
        (display "this is compiled proc1, called by compound proc, arguments: ")
        (displayln (list a b c))
        "return value"
       )
    )
    (define 
      (compound-proc a b c)
      (display "this is compound proc, called by compiled proc2, arguments: ")
      (displayln (list a b c))
      (compiled-proc1 7 8 9)
    )
    (compile-and-run 
      '(define
        (compiled-proc2 a b c)
        (display "this is compiled proc2, arguments: ")
        (displayln (list a b c))
        (compound-proc 4 5 6)
       )
    )
    (compiled-proc2 1 2 3)
   )
)

(set-register-contents! eceval 'flag false)
(start eceval)
