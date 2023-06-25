#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "m-eval/eval-apply.rkt")
(require "m-eval/global-environment.rkt")

(define a-val actual-value)

; main
(a-val 
  '(define
    (try a b)
    (if (= a 0) 1 b)
   )
  the-global-environment
)
; 'ok

; 遅延評価をサポートしている
(a-val '(try 0 (/ 1 0)) the-global-environment)
; 1

(a-val '(define count 0) the-global-environment)
; 'ok
(a-val 
  '(define
    (id x)
    (set! count (+ count 1))
    x
   )
  the-global-environment
)
; 'ok
(a-val '(define w (id (id 10))) the-global-environment)
; 'ok
; ここでwの値が評価されるとき、idの被演算子である(id 10)は遅延される。
; 一方、演算子であるidのbodyは評価されて、countは1になる。id手続きはxをそのまま返すため、wは遅延化された(id 10)になる。

(a-val 'count the-global-environment)
; 1
(a-val 'w the-global-environment)
; 10
; wの値をreplが（このファイルプログラムではa-valが）forceするため、遅延化されていた(id 10)が実行されてcountが2になる。
(a-val 'count the-global-environment)
; 12
