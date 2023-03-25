#lang racket/base
(define 
  (sum term a next b)
  (if (> a b) 
    0
    (+ (term a) 
       (sum term (next a) next b)
    )
  )
)

(define 
  (integral f a b dx)

  (define 
    (add-dx x)
    (+ x dx)
  )

  (* 
    (sum 
      f
      (+ a (/ dx 2.0))
      add-dx
      b
    )
    dx
  )
)

(define 
  (simpson f a b n)

  (define 
    h
    (/ (- b a) n)
  )

  (define 
    (term k)

    (define 
      (even? n)
      (= (remainder n 2) 0)
    )

    (* 
      (cond 
        ((= k 0) 1)
        ((= k n) 1)
        ((even? k) 2)
        (else 4)
      )
      (f (+ a (* k h)))
    )
  )

  (define 
    (next x)
    (+ x 1)
  )

  (* 
    (/ h 3.0)
    (sum term 0 next n)
  )
)

(define (cube x) (* x x x))

(integral cube 0 1 0.01)
; 0.24998750000000042
(integral cube 0 1 0.001)
; 0.249999875000001

(simpson cube 0 1 100)
; 0.25
(simpson cube 0 1 1000)
; 0.25

; integralとsimpsonのそれぞれの処理のステップ数は同じである。しかし近似の精度についてはsimpsonの方が良い
