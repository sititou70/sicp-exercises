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
  message
  '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah 
    yip yip yip yip yip yip yip yip yip Sha boom
   )
)
(define 
  huffman-tree
  (generate-huffman-tree 
    '((a 2) (Get 2) (Sha 3) (Wah 1) (boom 1) (job 2) (na 16) (yip 9))
  )
)

(define encoded (encode message huffman-tree))
encoded
; '(1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 
;   1 0 0 0 0 0 0 0 0 0 1 1 0 1 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 0
; )

; エンコード後のビット数は
(length encoded)
; 84
; したがって、最低でも84ビット必要

; もし固定長符号をこの8記号に使う場合、1記号あたり3ビット長なので
(* (length message) 3)
; 108
; ビット必要になる

(decode encoded huffman-tree)
; '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah
;   yip yip yip yip yip yip yip yip yip Sha boom
; )
