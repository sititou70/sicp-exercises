#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "m-eval/eval-apply.scm")
(require "m-eval/global-environment.scm")

; main
; 4.1節のメタ評価器の構文を拡張して、unlessを実装する
(eval 
  '(define
    (factorial n)
    (unless 
      (= n 1)
      (* n (factorial (- n 1)))
      1
    )
   )
  the-global-environment
)
; 'ok
(eval '(factorial 5) the-global-environment)
; 120

; Alyssaは、unlessを構文として組み込むと、高階手続きと組み合わせられなくなると指摘した。
; これはつまり、unlessを手続きの入力や出力にできなくなるということである。
; 例えばunlessの各引数の値のリストがあったとして、それらをunlessでマップするという場合を考える。

; (define conditions '(#t #t #f #t #f))
; (define usual-values '(1 2 3 4 5))
; (define exceptional-values '("error 1" "error 2" "error 3" "error 4" "error 5"))
; (map unless conditions usual-values exceptional-values) ; これはできない

; unlessを構文として実装してしまうと、mapにunlessを入力することができない。
