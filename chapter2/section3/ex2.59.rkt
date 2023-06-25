#lang racket/base

(define 
  (element-of-set? x set)
  (cond 
    ((null? set) #f)
    ((equal? x (car set)) #t)
    (else (element-of-set? x (cdr set)))
  )
)

(define 
  (adjoin-set x set)
  (if (element-of-set? x set) 
    set
    (cons x set)
  )
)

(define 
  (union-set set1 set2)
  (cond 
    ((null? set1) set2)
    ((null? set2) set1)
    ((element-of-set? (car set1) set2)
     (union-set (cdr set1) set2)
    )
    (else (cons (car set1) (union-set (cdr set1) set2)))
  )
)

(union-set '() '())
; '()
(union-set '(a) '())
; '(a)
(union-set '(a) '(b))
; '(a b)
(union-set '(a b) '(b))
; '(a b)
(union-set '(a b) '(b c))
; '(a b c)
