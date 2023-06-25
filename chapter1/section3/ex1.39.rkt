#lang racket/base

; result: iより大きい添字の部分を計算した結果（現在の分母の分数部分）
(define 
  (cont-frac-iter-sub n d i result)

  (let 
    ((next-result (/ (n i) (- (d i) result))))

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

(define 
  (tan-cf x k)

  (define 
    (square x)
    (* x x)
  )

  (cont-frac-iter 
    (lambda (i) (if (= i 1) x (square x)))
    (lambda (i) (- (* i 2) 1))
    k
  )
)

(define pi 3.1415926535897932384626433832795)
(define 
  (deg2rad x)
  (* pi (/ x 180))
)
(tan-cf (deg2rad 0) 100)
; 0
(tan-cf (deg2rad 30) 100)
; 0.5773502691896257
(tan-cf (deg2rad 45) 100)
; 1.0
(tan-cf (deg2rad 60) 100)
; 1.732050807568877
