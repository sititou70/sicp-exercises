#lang racket/base

(define (cube x) (* x x x))
(define 
  (p x)
  (- (* 3 x) 
     (* 4 (cube x))
  )
)
(define 
  (sine angle)
  (if (not (> (abs angle) 0.1)) 
    angle
    (p (sine (/ angle 3.0)))
  )
)

(sine 12.15)
; を評価する際に，pは5回呼ばれる

; sineの空間とステップ数の増加オーダーについて，
; sineはangleを3で割っていき，それが0.1よりも小さくなったときに停止する．
; 呼び出し回数をCとしてこれを数式で表すと，
; angle / 3^C < 0.1
; angle / 0.1 < 3^C
; log(angle / 0.1) < log(3^C)
; log(angle ) - log(0.1) < C * log(3)
; (log(angle ) - log(0.1)) / log(3) < C
; となる．したがって，増加オーダーはΘ(log(n))となる．
