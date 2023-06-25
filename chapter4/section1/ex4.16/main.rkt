#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "m-eval/eval-apply.rkt")
(require "m-eval/global-environment.rkt")

; main
; 評価器はletをサポートしている
(eval 
  '(let
    ( ;
     (a 1)
    )
    a
   )
  the-global-environment
)
; 1

; defineを含む式は
(eval 
  '(lambda () 
     (define u 1)
     (define v 2)
     (cons u v)
   )
  the-global-environment
)
; 以下のように変換が行われているとわかる
; (list 
;   'procedure
;   '()
;   (list 
;     (mcons 
;       'let
;       (mcons 
;         ; binds
;         (list 
;           (list 
;             'v
;             (list 'quote '*unassigned*)
;           )
;           (list 
;             'u
;             (list 'quote '*unassigned*)
;           )
;         )
; 
;         ; body
;         (list 
;           (list 'set! 'u 1)
;           (list 'set! 'v 2)
;           (list 'cons 'u 'v)
;         )
;       )
;     )
;   )
;   'env
; )

; 実際に適用すると
(eval 
  '((lambda () 
      (define u 1)
      (define v 2)
      (cons u v)
    )
   )
  the-global-environment
)
; (mcons 1 2)

; scan-out-definesはprocedure-bodyではなくmake-procedureに組み込んだ。
; 一般的に、手続きの定義は1回しか行われないのに対して、適用は何度も行われる可能性がある。
; make-procedureは手続きの定義時に呼ばれるのに対して、procedure-bodyは手続きの適用時に何度も呼ばれる可能性がある
; したがって、パフォーマンスの観点から、scan-out-definesはmake-procedureに組み込むほうが良いと判断した。
