#lang racket/base

(define 
  (cons a b)
  (* (expt 2 a) (expt 3 b))
)

; nに因数factorが何個含まれるか数える
(define 
  (count-factors n factor)

  (define 
    (iter n cnt)
    (if (= (remainder n factor) 0) 
      (iter 
        (/ n factor)
        (+ cnt 1)
      )
      cnt
    )
  )

  (iter n 0)
)
(define 
  (car n)
  (count-factors n 2)
)
(define 
  (cdr n)
  (count-factors n 3)
)

(car (cons 1 2))
; 1
(cdr (cons 1 2))
; 2

(car (cons 12 34))
; 12
(cdr (cons 12 34))
; 34
