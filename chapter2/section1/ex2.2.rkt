#lang racket/base

(define 
  (average x y)
  (/ (+ x y) 2)
)

(define 
  (make-point x y)
  (cons x y)
)
(define 
  (x-point p)
  (car p)
)
(define 
  (y-point p)
  (cdr p)
)

(define 
  (make-segment start end)
  (cons start end)
)
(define 
  (start-segment seg)
  (car seg)
)
(define 
  (end-segment seg)
  (cdr seg)
)

(define 
  (midpoint-segment seg)
  (let 
    ((start (start-segment seg)) 
      (end (end-segment seg))
    )
    (make-segment 
      (average (x-point start) (x-point end))
      (average (y-point start) (y-point end))
    )
  )
)

(define 
  (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
)

(print-point 
  (midpoint-segment 
    (make-segment 
      (make-point -10 -10)
      (make-point 10 10)
    )
  )
)
; (0, 0)
