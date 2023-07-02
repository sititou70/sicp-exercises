#lang racket
(provide (all-defined-out))

(require sicp)
(require "machine/make-machine.rkt")
(require "ec-eval/repl.rkt")
(require "ec-eval/eval-apply.rkt")

; main
(define 
  eceval
  (make-machine 
    '(exp env val proc argl continue unev)
    (append repl-operations eval-operations apply-operations)
    (append repl-insts eval-insts apply-insts)
  )
)

(start eceval)
