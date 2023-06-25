#lang racket/base

(define 
  (inc x)
  (+ x 1)
)

(define 
  (double f)
  (lambda (x) (f (f x)))
)

((double inc) 1)
; 3

(((double (double double)) inc) 5)
; 21
; (double (double double))は、doubleをdoubleして、さらにdoubleするという意味である。
; これはdouble(double(double(double(inc))))に等しいので、incは2^4回実行される
