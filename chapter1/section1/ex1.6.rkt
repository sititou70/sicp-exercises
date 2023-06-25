#lang racket/base

(define 
  (new-if predicate then-clause else-clause)
  (cond 
    (predicate then-clause)
    (else else-clause)
  )
)

(define 
  (average x y)
  (/ (+ x y) 2)
)

(define 
  (square x)
  (* x x)
)

(define 
  (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001)
)

(define 
  (improve guess x)
  (average guess (/ x guess))
)

(define 
  (sqrt-iter guess x)
  (new-if 
    (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x) ; ここが述語の真偽によらず常に評価される
  )
)

(define 
  (sqrt x)
  (sqrt-iter 1.0 x)
)

(sqrt 9)
; この式の評価は停止しない．
; Schemeは適用順序評価を用いているため，new-ifの被演算子は常に評価される．
; したがって，sqrt-iterが常に評価されるため，近似が停止しない
