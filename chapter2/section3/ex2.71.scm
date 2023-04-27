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

(generate-huffman-tree 
  '((a 1)
    (b 2)
    (c 4)
    (d 8)
    (e 16)
   )
)
;'(
;   (
;     (
;       (
;         (leaf a 1) 
;         (leaf b 2)
;         (a b)
;         3
;       ) 
;       (leaf c 4)
;       (a b c)
;       7
;     ) 
;     (leaf d 8)
;     (a b c d)
;     15
;   ) 
;   (leaf e 16)
;   (a b c d e)
;   31
; )

; 図示すると
;     .
;    . e
;   . d
;  . c
; a b

(generate-huffman-tree 
  '((a 1)
    (b 2)
    (c 4)
    (d 8)
    (e 16)
    (f 32)
    (g 64)
    (h 128)
    (i 256)
    (j 512)
   )
)
;'(
;   (
;     (
;       (
;         (
;           (
;             (
;               (
;                 (
;                   (leaf a 1) 
;                   (leaf b 2)
;                   (a b)
;                   3
;                 ) 
;                 (leaf c 4)
;                 (a b c)
;                 7
;               ) 
;               (leaf d 8)
;               (a b c d)
;               15
;             ) 
;             (leaf e 16)
;             (a b c d e)
;             31
;           ) 
;           (leaf f 32)
;           (a b c d e f)
;           63
;         ) 
;         (leaf g 64)
;         (a b c d e f g)
;         127
;       ) 
;       (leaf h 128)
;       (a b c d e f g h)
;       255
;     ) 
;     (leaf i 256)
;     (a b c d e f g h i)
;     511
;   ) 
;   (leaf j 512)
;   (a b c d e f g h i j)
;   1023
; )

; 図示すると
;          .
;         . j
;        . i
;       . h
;      . g
;     . f
;    . e
;   . d
;  . c
; a b

; 一般のnについて、最も高頻度の記号は符号化に1ビット必要である
; 最も低頻度の記号は符号化に
; 1 (n = 1)
; n - 1 (otherwise)
; ビット必要である
