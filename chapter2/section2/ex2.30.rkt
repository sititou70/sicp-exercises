#lang racket/base

(define 
  (square x)
  (* x x)
)

(define 
  (square-tree1 tree)

  (cond 
    ((null? tree) null)
    ((pair? (car tree))
     (cons 
       (square-tree1 (car tree))
       (square-tree1 (cdr tree))
     )
    )
    (else
     (cons 
       (square (car tree))
       (square-tree1 (cdr tree))
     )
    )
  )
)

(square-tree1 
  (list 
    1
    (list 2 (list 3 4) 5)
    (list 6 7)
  )
)
; (1 (4 (9 16) 25) (36 49))

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
  (square-tree2 tree)

  (map 
    (lambda (sub-tree) 
      (if (pair? sub-tree) 
        (square-tree2 sub-tree)
        (square sub-tree)
      )
    )
    tree
  )
)

(square-tree2 
  (list 
    1
    (list 2 (list 3 4) 5)
    (list 6 7)
  )
)
; (1 (4 (9 16) 25) (36 49))
