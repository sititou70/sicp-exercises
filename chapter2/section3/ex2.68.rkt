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
  (decode bits tree)

  (define 
    (choose-branch bit branch)
    (cond 
      ((= bit 0) (left-branch branch))
      ((= bit 1) (right-branch branch))
      (else (error "bad bit: CHOOSE-BRANCH " bit))
    )
  )

  (define 
    (decode-1 bits current-branch)
    (if (null? bits) 
      '()
      (let 
        ((next-branch 
           (choose-branch (car bits) current-branch)
         ) 
        )
        (if (leaf? next-branch) 
          (cons (symbol-leaf next-branch) 
                (decode-1 (cdr bits) tree)
          )
          (decode-1 (cdr bits) next-branch)
        )
      )
    )
  )

  (decode-1 bits tree)
)

(define 
  (encode message tree)

  (define 
    (encode-symbol symbol tree)

    (define 
      (iter tree path)
      (if (leaf? tree) 
        path
        (cond 
          ((and 
             (not (null? (left-branch tree)))
             (memq symbol (symbols (left-branch tree)))
           )
           (iter (left-branch tree) (append path '(0)))
          )
          ((and 
             (not (null? (right-branch tree)))
             (memq symbol (symbols (right-branch tree)))
           )
           (iter (right-branch tree) (append path '(1)))
          )
          (else (error "unknown symbol!"))
        )
      )
    )

    (iter tree '())
  )

  (if (null? message) 
    '()
    (append (encode-symbol (car message) tree) 
            (encode (cdr message) tree)
    )
  )
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

(encode '(A D A B B C A) sample-tree)
;                      '(0 1 1 0 0 1 0 1 0 1 1 1 0)
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode (encode '(A B C D D C B A) sample-tree) sample-tree)
; '(A B C D D C B A)

(encode '(E) sample-tree)
; unknown symbol!
