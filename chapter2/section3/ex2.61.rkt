#lang racket/base

(define 
  (adjoin-set x set)
  (cond 
    ((null? set) (list x))
    ((= x (car set)) set)
    ((< x (car set)) (cons x set))
    (else
     (cons 
       (car set)
       (adjoin-set x (cdr set))
     )
    )
  )
)

(adjoin-set 3 '())
; '(3)
(adjoin-set 3 '(1 2))
; '(1 2 3)
(adjoin-set 3 '(4 5))
; '(3 4 5)
(adjoin-set 3 '(2 4))
; '(2 3 4)
