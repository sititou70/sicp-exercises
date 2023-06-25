#lang racket/base

(define 
  (reverse items)

  (if (null? items) 
    null
    (append (reverse (cdr items)) (list (car items)))
  )
)

(reverse (list 1 4 9 16 25))
; '(25 16 9 4 1)
