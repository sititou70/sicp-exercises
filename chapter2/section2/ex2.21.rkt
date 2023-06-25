#lang racket/base

(define 
  (square x)
  (* x x)
)

(define 
  (square-list1 items)
  (if (null? items) 
    null
    (cons (square (car items)) (square-list1 (cdr items)))
  )
)

(define 
  (map proc items)
  (if (null? items) 
    null
    (cons (proc (car items)) 
          (map proc (cdr items))
    )
  )
)
(define 
  (square-list2 items)
  (map (lambda (x) (square x)) items)
)

(square-list1 (list 1 2 3 4))
; '(1 4 9 16)
(square-list2 (list 1 2 3 4))
; '(1 4 9 16)
