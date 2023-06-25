#lang sicp

(define 
  (count-pairs x)
  (if (not (pair? x)) 
    0
    (+ (count-pairs (car x)) 
       (count-pairs (cdr x))
       1
    )
  )
)

(define l1 (cons 1 (cons 2 (cons 3 nil))))
(count-pairs l1)
; 3

(define l2-1 (cons '1 '2))
(define l2-2 (cons l2-1 l2-1))
(define l2 (cons l2-2 '3))
(count-pairs l2)
; 4

(define l3-1 (cons '1 '2))
(define l3-2 (cons l3-1 l3-1))
(define l3 (cons l3-2 l3-2))
(count-pairs l3)
; 7

(define l4 (cons 1 (cons 2 (cons 3 nil))))
(set-cdr! (cddr l4) l4)
(count-pairs l4)
; 停止しない
