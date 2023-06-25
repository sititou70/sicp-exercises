#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "m-eval/eval-apply.rkt")
(require "m-eval/global-environment.rkt")

(define a-val actual-value)

; main
; 演算子がthunkである場合があるため、強制が必要である。例えば
(a-val '(define (id x) x) the-global-environment)
(a-val '(define op (id +)) the-global-environment)

; ここで、opは遅延化された+演算子である
(eval 'op the-global-environment)
; (mcons 'thunk (mcons '+ ...

; したがって、以下の式を評価するためには、演算子を強制する必要がある。
(a-val '(op 1 2) the-global-environment)
; 3
