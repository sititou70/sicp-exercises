#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "eval-apply.rkt")
(require "global-environment.rkt")

; main

; a. Louis の計画はどこがまずいのだろうか
; Louisの計画では、手続き適用以降のタグを持たない構文を適切に評価できない。
; 例えば、 (define x 3)という式は、defineという手続きを適用するものと評価されてしまう。

; b. 評価対象の言語の構文を変更し、手続き適用が call で始まるようすることで、彼の手伝いをせよ。
(eval 
  '(define
    (factorial x)
    (if (call = x 0) 
      1
      (call * x (call factorial (call - x 1)))
    )
   )
  the-global-environment
)
; 'ok

(eval '(call factorial 7) the-global-environment)
; 5040
