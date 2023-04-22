#lang racket/base

(define 
  (equal? a b)
  (if (and (pair? a) (pair? b)) 
    (and 
      (equal? (car a) (car b))
      (equal? (cdr a) (cdr b))
    )
    (eq? a b)
  )
)

(equal? '(this is a list) '(this is a list))
; #t

(equal? '(this is a list) '(this (is a) list))
; #f

(equal? '(this (is a) list) '(this (is a) list))
; #t
