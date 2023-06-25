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

(sqrt 9)
; 3.00009155413138

(sqrt (+ 100 37))
; 11.704699917758145

(sqrt (+ (sqrt 2) (sqrt 3)))
; 1.7739279023207892

(square (sqrt 1000))
; 1000.000369924366
