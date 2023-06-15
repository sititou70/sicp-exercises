#lang racket
(provide (all-defined-out))

(require sicp)
(require "q-eval/repl.scm")

(define repl (make-repl 'display))

; main
(repl 
  '
  (assert! 
    (rule 
      (same ?x ?x)
    )
  )
)
; 規則1
(repl 
  '
  (assert! 
    (rule 
      (last-pair (?first1 . ?rest1) (?first2 . ?rest2))
      (and 
        (same ?rest1 ())
        (same ?rest2 ())
        (same ?first1 ?first2)
      )
    )
  )
)
; 規則2
(repl 
  '
  (assert! 
    (rule 
      (last-pair (?first1 . ?rest1) ?last)
      (and 
        (not (same ?rest1 ()))
        (last-pair ?rest1 ?last)
      )
    )
  )
)

(repl '(last-pair (3) ?x))
; {last-pair {3} {3}}
(repl '(last-pair (1 2 3) ?x))
; {last-pair {1 2 3} {3}}
(repl '(last-pair (2 ?x) (3)))
; {last-pair {2 3} {3}}

; リストが不明なクエリに対しては以下のようになる。
(repl '(last-pair ?x (3)))
; {last-pair {3} {3}}

; なぜこのような結果が得られたのかを述べる。

; まず、規則1では、次のようにユニフィケーションが進む。

; last-pair規則によって
; rest2(last-pair) = ()
; first2(last-pair) = 3
; x(repl) = (first1(last-pair) . rest1(last-pair))

; same規則によって
; x(same1) = ()
; rest1(last-pair) = x(same1)
; rest2(last-pair) = ()
; first2(last-pair) = 3
; x(repl) = (first1(last-pair) . rest1(last-pair))

; same規則によって
; x(same2) = ()
; x(same1) = ()
; rest1(last-pair) = x(same1)
; rest2(last-pair) = ()
; first2(last-pair) = 3
; x(repl) = (first1(last-pair) . rest1(last-pair))

; same規則によって
; x(same3) = 3
; first1(last-pair) = x(same3)
; x(same2) = ()
; x(same1) = ()
; rest1(last-pair) = x(same1)
; rest2(last-pair) = ()
; first2(last-pair) = 3
; x(repl) = (first1(last-pair) . rest1(last-pair))

; したがって、xをインスタンス化すると{3}になる。

; 次に、規則2では次のようにユニフィケーションが進む。

; last-pair規則によって
; last(last-pair) = (3)
; x(repl) = (first1(last-pair) . rest1(last-pair))

; same規則によって
; x(same) = ()
; rest1(last-pair) = x(same)
; last(last-pair) = (3)
; x(repl) = (first1(last-pair) . rest1(last-pair))

; ここで、same規則のユニフィケーションに成功したため、notによって以降は空のフレームとなる。

; 以上をまとめると、結局得られる結果は{last-pair {3} {3}}だけである。
; 3を末尾に持つあらゆるリストが得られるわけではない。

