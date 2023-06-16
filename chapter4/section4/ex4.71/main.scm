#lang racket
(provide (all-defined-out))

(require sicp)
(require "q-eval/repl.scm")
(require "q-eval/eval-apply.scm")

(define repl (make-repl 'display))

; main
; 一番の違いは無限ループするクエリの場合に現れる。
; 本文の実装では、規則の適用やorの残りの句の評価は遅延されていた。
; そのため、たとえ無限ループになってしまっても、途中の結果が表示されることがあった。
(repl '(assert! (son Adam Cain)))
(repl 
  '
  (assert! 
    (rule 
      (son 'x 'y)
      (son 'x 'y)
    )
  )
)

; (repl '(son ?x ?y))
; {son Adam Cain}
; 以降停止しない

; 一方、Louisの実装では、適用可能な規則やorの句が無限にある場合、それらの評価が完了してからでないと結果が表示されない。
(use-louis-proc)
(repl '(son ?x ?y))
; 何も表示されず停止しない
