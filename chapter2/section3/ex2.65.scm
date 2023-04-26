#lang racket/base

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define 
  (make-tree entry left right)
  (list entry left right)
)

(define 
  (list-union-set set1 set2)
  (cond 
    ((null? set1) set2)
    ((null? set2) set1)
    (else
     (let 
       ((x1 (car set1)) (x2 (car set2)))
       (cond 
         ((= x1 x2)
          (cons 
            x1
            (list-union-set 
              (cdr set1)
              (cdr set2)
            )
          )
         )
         ((< x1 x2)
          (cons 
            x1
            (list-union-set 
              (cdr set1)
              set2
            )
          )
         )
         ((< x2 x1)
          (cons 
            x2
            (list-union-set 
              set1
              (cdr set2)
            )
          )
         )
       )
     )
    )
  )
)

(define 
  (list-intersection-set set1 set2)
  (if (or (null? set1) (null? set2)) 
    '()
    (let 
      ((x1 (car set1)) (x2 (car set2)))
      (cond 
        ((= x1 x2)
         (cons x1 
               (list-intersection-set 
                 (cdr set1)
                 (cdr set2)
               )
         )
        )
        ((< x1 x2)
         (list-intersection-set (cdr set1) set2)
        )
        ((< x2 x1)
         (list-intersection-set set1 (cdr set2))
        )
      )
    )
  )
)

(define 
  (tree->list tree)
  (define 
    (copy-to-list tree result-list)
    (if (null? tree) 
      result-list
      (copy-to-list 
        (left-branch tree)
        (cons (entry tree) 
              (copy-to-list 
                (right-branch tree)
                result-list
              )
        )
      )
    )
  )
  (copy-to-list tree '())
)

(define 
  (list->tree elements)

  (define 
    (partial-tree elts n)
    (if (= n 0) 
      (cons '() elts)
      (let 
        ((left-size (quotient (- n 1) 2)))
        (let 
          ((left-result 
             (partial-tree elts left-size)
           ) 
          )
          (let 
            ((left-tree (car left-result)) 
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1)))
            )
            (let 
              ((this-entry (car non-left-elts)) 
                (right-result 
                  (partial-tree 
                    (cdr non-left-elts)
                    right-size
                  )
                )
              )
              (let 
                ((right-tree (car right-result)) 
                  (remaining-elts 
                    (cdr right-result)
                  )
                )
                (cons 
                  (make-tree 
                    this-entry
                    left-tree
                    right-tree
                  )
                  remaining-elts
                )
              )
            )
          )
        )
      )
    )
  )

  (car (partial-tree elements (length elements)))
)

(define 
  (union-set tree1 tree2)
  (list->tree (list-union-set (tree->list tree1) (tree->list tree2)))
)
(define 
  (intersection-set tree1 tree2)
  (list->tree (list-intersection-set (tree->list tree1) (tree->list tree2)))
)

(tree->list 
  (union-set 
    (list->tree '(1 4 9 16 25))
    (list->tree '(1 3 5 7 9 11))
  )
)
; '(1 3 4 5 7 9 11 16 25)
(tree->list 
  (intersection-set 
    (list->tree '(1 4 9 16 25))
    (list->tree '(1 3 5 7 9 11))
  )
)
; '(1 9)
