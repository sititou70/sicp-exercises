#lang racket/base

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
  (if (good-enough? guess x) 
    guess
    (sqrt-iter (improve guess x) x)
  )
)

(define 
  (sqrt x)
  (sqrt-iter 1.0 x)
)

(sqrt 0.0001)
; 0.0001という小さい数の平方根を求める場合を考える．
; この解は0.01だが，実際には
; 0.03230844833048122
; という出力になってしまう．これは適切ではない
; これは，good-enough?が以下のように評価されるからである
;     (good-enough? 0.03230844833048122 0.0001)
;     (< (abs (- (square 0.03230844833048122) 0.0001)) 0.001)
;     (< (abs (- 0.00104383583 0.0001)) 0.001)
;     (< (abs 0.00094383583) 0.001)
;     (< 0.00094383583 0.001)
;     #t
; つまり，0.001という決め打ちの値が，0.0001という被開平数に対して大きすぎるため，十分でない近似となってしまった

(sqrt 1000000)
; 1000000という大きな数の平方根を求める場合を考える．この場合，以下のような結果が得られる
; 1000.0000000000118
; 小さい数の議論を踏まえると，この結果は近似されすぎているといえる．
; 被開平数によって近似の精度が変わってしまうのは，不便であり適切でない

; 問題文に示されているgood-enough?のもう1つの戦略は上記の問題に対して有効である．
; 以下にその実装を示す．ここでは，変化が推定値の0.1%以下になったとき近似が停止する
(define 
  (good-enough?2 prev-guess guess x)
  (if (boolean? prev-guess) 
    #f
    (< (abs (- prev-guess guess)) (* guess 0.001))
  )
)

(define 
  (sqrt-iter2 prev-guess guess x)
  (if 
    (or 
      (not (good-enough?2 prev-guess guess x))
      (boolean? prev-guess)
    )
    (sqrt-iter2 guess (improve guess x) x)
    guess
  )
)

(define 
  (sqrt2 x)
  (sqrt-iter2 #f 1.0 x)
)

; 以下の値が得られる

(sqrt2 0.0001)
; 0.010000000025490743

(sqrt2 1000000)
; 1000.0001533016629
