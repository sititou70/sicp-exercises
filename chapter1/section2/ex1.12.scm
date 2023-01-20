#lang racket/base

; パスカルの三角形のn行m列の要素を計算する
;     0   1   2   3   4   m列
; 0   1   1   1   1   1
; 1   1   2   3   4   5
; 2   1   3   6   10  15
; 3   1   4   10  20  35
; 4   1   5   15  35  70
; n行
(define 
  (f n m)
  (if (or (<= n 0) (<= m 0)) 
    1
    (+ 
      (f n (- m 1))
      (f (- n 1) m)
    )
  )
)

(display (f 0 0))
(display "\t")
(display (f 0 1))
(display "\t")
(display (f 0 2))
(display "\t")
(display (f 0 3))
(display "\t")
(display (f 0 4))
(display "\n")

(display (f 1 0))
(display "\t")
(display (f 1 1))
(display "\t")
(display (f 1 2))
(display "\t")
(display (f 1 3))
(display "\t")
(display (f 1 4))
(display "\n")

(display (f 2 0))
(display "\t")
(display (f 2 1))
(display "\t")
(display (f 2 2))
(display "\t")
(display (f 2 3))
(display "\t")
(display (f 2 4))
(display "\n")

(display (f 3 0))
(display "\t")
(display (f 3 1))
(display "\t")
(display (f 3 2))
(display "\t")
(display (f 3 3))
(display "\t")
(display (f 3 4))
(display "\n")

(display (f 4 0))
(display "\t")
(display (f 4 1))
(display "\t")
(display (f 4 2))
(display "\t")
(display (f 4 3))
(display "\t")
(display (f 4 4))
(display "\n")
