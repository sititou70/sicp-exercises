#lang racket
(provide (all-defined-out))

(require sicp)
(require "q-eval/repl.scm")

(define repl (make-repl 'display))

; main
; ストリームを互い違いに挟み込むことによって、片方のストリームがとても長い場合や、取得に時間がかかる場合にも望ましい挙動になる。

(repl 
  '
  (assert! 
    (rule 
      (same ?x ?x)
    )
  )
)
(repl 
  '
  (assert! 
    (rule 
      (element-of (?first . ?rest) ?e)
      (or 
        (same ?first ?e)
        (element-of ?rest ?e)
      )
    )
  )
)
(repl 
  '
  (assert! 
    (rule 
      (element-of-slow (?first . ?rest) ?e)
      (and 
        (lisp-value (lambda () (sleep 1) #t))
        (or 
          (same ?first ?e)
          (element-of-slow ?rest ?e)
        )
      )
    )
  )
)
(repl 
  '
  (assert! 
    (rule 
      (integers ?e)
      (or 
        ; 以下のルールは取得に長い時間がかかる
        (element-of-slow 
          (1 3 5 7 9 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30)
          ?e
        )
        ; こちらは時間がかからない
        (element-of (2 4 6 8 10) ?e)
      )
    )
  )
)

(repl '(integers ?e))
; {integers 1}
; {integers 2}
; {integers 3}
; {integers 4}
; {integers 5}
; {integers 6}
; {integers 7}
; {integers 8}
; {integers 9}
; {integers 10}
; さらに続く...

; もし実装が単なるappendで行われていたら、{integers 10}の結果を得るには、遅い方のストリームをすべて取得してからでないといけなかった。
