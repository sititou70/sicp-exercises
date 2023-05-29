#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "repl.scm")
(require "eval-apply.scm")

; main
(eval '(define a 'none) the-global-environment)
; 'ok

(eval '(cons (set! a 'left) (set! a 'right)) the-global-environment)
; (mcons 'ok 'ok)
(eval 'a the-global-environment)
; 'right
; 基底のLisp実装（Racket）では、被演算子は左から評価される（右が後に評価される）

; list-of-values-leftを使用した場合
(use-list-of-values-left)
(eval '(cons (set! a 'left) (set! a 'right)) the-global-environment)
; (mcons 'ok 'ok)
(eval 'a the-global-environment)
; 'right
; 基底のLisp実装にかかわらず、被演算子は左から評価される

; list-of-values-rightを使用した場合
(use-list-of-values-right)
(eval '(cons (set! a 'left) (set! a 'right)) the-global-environment)
; (mcons 'ok 'ok)
(eval 'a the-global-environment)
; 'left
; 基底のLisp実装にかかわらず、被演算子は右から評価される
