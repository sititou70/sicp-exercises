#lang racket/base

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define 
  (make-tree entry left right)
  (list entry left right)
)

(define 
  (lookup x set)
  (cond 
    ((null? set) #f)
    ((= x (entry set)) (entry set))
    ((< x (entry set))
     (lookup x (left-branch set))
    )
    ((> x (entry set))
     (lookup x (right-branch set))
    )
  )
)

(define 
  tree
  (make-tree 
    3
    (make-tree 1 null null)
    (make-tree 
      7
      (make-tree 5 null null)
      (make-tree 
        9
        null
        (make-tree 11 null null)
      )
    )
  )
)
(lookup 3 tree)
; 3
(lookup 11 tree)
; 11
(lookup 6 tree)
; #f
