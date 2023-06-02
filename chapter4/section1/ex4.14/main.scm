#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "m-eval/eval-apply.scm")
(require "m-eval/global-environment.scm")

; main
; Evaのアプローチはうまくいく
(eval 
  '
  (define 
    (eva)

    (define 
      (map proc items)
      (if (null? items) 
        '()
        (cons 
          (proc (car items))
          (map proc (cdr items))
        )
      )
    )
    (map (lambda (x) x) '(1 2 3))
  )
  the-global-environment
)
(eval '(eva) the-global-environment)
; 'ok
; (mcons 1 (mcons 2 (mcons 3 '())))

; 一方、Louisのアプローチはうまく行かない。
; 基底システムのmapは、メタ循環評価器の内部表現である複合手続きを受け付けないからである。
(eval '(map (lambda (x) x) '(1 2 3)) the-global-environment)
; application: not a procedure;
