#lang racket/base
(require racket/trace)

(define 
  (cont-frac-rec-sub n d i k)

  (if (= i k) 
    (/ (n i) (d i))
    (/ 
      (n i)
      (+ 
        (d i)
        (cont-frac-rec-sub 
          n
          d
          (+ i 1)
          k
        )
      )
    )
  )
)
(define 
  (cont-frac-rec n d k)

  (cont-frac-rec-sub n d 1 k)
)

; result: iより大きい添字の部分を計算した結果（現在の分母の分数部分）
(define 
  (cont-frac-iter-sub n d i result)

  (let 
    ((next-result (/ (n i) (+ (d i) result))))

    (if (= i 1) 
      next-result
      (cont-frac-iter-sub 
        n
        d
        (- i 1)
        next-result
      )
    )
  )
)
(define 
  (cont-frac-iter n d k)

  (cont-frac-iter-sub n d k 0)
)

; 再帰的および反復的なcont-fracの実行結果を示す
(trace cont-frac-rec-sub cont-frac-iter-sub)
(cont-frac-rec 
  (lambda (i) 1.0)
  (lambda (i) 1.0)
  10
)
(cont-frac-iter 
  (lambda (i) 1.0)
  (lambda (i) 1.0)
  10
)
(untrace cont-frac-rec-sub cont-frac-iter-sub)

; 小数点以下4桁の精度を得るためのkを考察する
; まず、1/φの真の値は：0.61803398874989484820...である

(cont-frac-iter 
  (lambda (i) 1.0)
  (lambda (i) 1.0)
  10
)
; 0.6179775280898876
(cont-frac-iter 
  (lambda (i) 1.0)
  (lambda (i) 1.0)
  11
)
; 0.6180555555555556
(cont-frac-iter 
  (lambda (i) 1.0)
  (lambda (i) 1.0)
  13
)
; 0.6180371352785146
; kが11のとき初めて少数第4位の値が正しくなる、また13のとき少数第5位の値が正しくなる。したがって、kは11〜13程度あれば十分である
