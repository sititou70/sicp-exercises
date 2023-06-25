#lang racket/base

(define 
  (fold-right op initial sequence)
  (if (null? sequence) 
    initial
    (op 
      (car sequence)
      (fold-right op initial (cdr sequence))
    )
  )
)

(define 
  (fold-left op initial sequence)
  (define 
    (iter result rest)
    (if (null? rest) 
      result
      (iter 
        (op result (car rest))
        (cdr rest)
      )
    )
  )
  (iter initial sequence)
)

(fold-right / 1 (list 1 2 3))
; 1 / (2 / 3) = 3/2
(fold-left / 1 (list 1 2 3))
; (1 / 2) / 3 = 1/6
(fold-right list null (list 1 2 3))
; (1 (2 (3 ())))
(fold-left list null (list 1 2 3))
; (((() 1) 2) 3)

; 一般的に、fold-rightとfold-leftが任意の列に対して同じ値を返すことを保証するためには、2項演算子opが結合的である必要がある
