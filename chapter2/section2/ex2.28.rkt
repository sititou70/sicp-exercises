#lang racket/base

(define 
  (fringe tree)

  (cond 
    ((null? tree) null)
    ((pair? (car tree))
     (append 
       (fringe (car tree))
       (fringe (cdr tree))
     )
    )
    (else
     (cons 
       (car tree)
       (fringe (cdr tree))
     )
    )
  )
)

(define x (list (list 1 2) (list 3 4)))
(fringe x)
; (1 2 3 4)

(fringe (list x x))
; (1 2 3 4 1 2 3 4)

(fringe (list (list x) (list x (list x) x)))
; (1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4)
