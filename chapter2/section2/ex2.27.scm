#lang racket/base

(define 
  (deep-reverse items)

  (define 
    (iter rest result)
    (cond 
      ((null? rest) result)
      ((pair? (car rest))
       (iter (cdr rest) (cons (deep-reverse (car rest)) result))
      )
      (else
       (iter (cdr rest) (cons (car rest) result))
      )
    )
  )

  (iter items null)
)

(define x (list (list 1 2) (list 3 4)))
(deep-reverse x)
; ((4 3) (2 1))
