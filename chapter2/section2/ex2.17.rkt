#lang racket/base

(define 
  (last-pair items)

  (if (null? (cdr items)) 
    (list (car items))
    (last-pair (cdr items))
  )
)

(last-pair (list 23 72 149 34))
; '(34)
