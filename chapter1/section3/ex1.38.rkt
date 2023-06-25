#lang racket/base

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

(+ 
  (cont-frac-iter 
    (lambda (i) 1.0)
    (lambda (i) 
      (if (= (remainder (+ i 1) 3) 0) 
        (* (/ (+ i 1) 3) 2)
        1
      )
    )
    100
  )
  2
)

