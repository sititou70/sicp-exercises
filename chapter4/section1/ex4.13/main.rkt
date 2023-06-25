#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "m-eval/eval-apply.rkt")
(require "m-eval/global-environment.rkt")

; main
; 特殊形式make-unboundは、フレームを内側からたどってはじめに見つけた束縛を削除する。
; これは、変数の参照メカニズムに対応しているため、直感的にわかりやすい。
; 例えば、(displayln x)は、「一番内側のフレームのxを表示」という意味であるのに対して、
; (make-unbound! x)は、「一番内側のフレームのxを削除」という意味になるため直感的である。

(eval '(define x 1) the-global-environment)
(eval 
  '(define
    (a)

    (define x 2)

    (define 
      (b)

      (define x 3)

      (displayln x)

      (make-unbound! x)
      (displayln x)

      (make-unbound! x)
      (displayln x)

      (make-unbound! x)
      (displayln x)
    )
    (b)
   )
  the-global-environment
)
; 'ok
(eval '(a) the-global-environment)
; 'ok
; 3
; 2
; 1
; Unbound variable  'x
