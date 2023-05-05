#lang racket/base

(define 
  (estimate-integral p x1 x2 y1 y2 trials)

  (define hits 0.0)

  (define 
    (random-in-range low high)
    (+ low (* high (random)))
  )

  (define 
    (iter rest-trials)
    (if (= rest-trials 0) 
      (* 
        (* (- x2 x1) (- y2 y1))
        (/ hits trials)
      )
      (begin 
        (if (p (random-in-range x1 x2) (random-in-range y1 y2)) 
          (set! hits (+ hits 1.0))
          'none
        )
        (iter (- rest-trials 1))
      )
    )
  )
  (iter trials)
)

(estimate-integral 
  (lambda (x y) (< (+ (* x x) (* y y)) 1))
  -1
  1
  -1
  1
  10000000
)
; 3.141378