#lang racket/base

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define 
  (make-code-tree left right)
  (list left 
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))
  )
)
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define 
  (symbols tree)
  (if (leaf? tree) 
    (list (symbol-leaf tree))
    (caddr tree)
  )
)
(define 
  (weight tree)
  (if (leaf? tree) 
    (weight-leaf tree)
    (cadddr tree)
  )
)

(define 
  (generate-huffman-tree pairs)

  (define 
    (adjoin-set x set)
    (cond 
      ((null? set) (list x))
      ((< (weight x) (weight (car set))) (cons x set))
      (else
       (cons (car set) 
             (adjoin-set x (cdr set))
       )
      )
    )
  )

  (define 
    (make-leaf-set pairs)
    (if (null? pairs) 
      '()
      (let 
        ((pair (car pairs)))
        (adjoin-set 
          (make-leaf 
            (car pair) ; symbol
            (cadr pair) ; frequency
          )
          (make-leaf-set (cdr pairs))
        )
      )
    )
  )

  (define 
    (successive-merge set)
    (if (= (length set) 1) 
      (car set)
      (let 
        ( ;
         (first (car set))
         (second (cadr set))
         (rest (cddr set))
        )

        (successive-merge 
          (adjoin-set 
            (make-code-tree first second)
            rest
          )
        )
      )
    )
  )

  (successive-merge (make-leaf-set pairs))
)

(define 
  sample-tree
  (make-code-tree 
    (make-leaf 'A 4)
    (make-code-tree 
      (make-leaf 'B 2)
      (make-code-tree 
        (make-leaf 'D 1)
        (make-leaf 'C 1)
      )
    )
  )
)
sample-tree
; '((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)

(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
; '((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)
