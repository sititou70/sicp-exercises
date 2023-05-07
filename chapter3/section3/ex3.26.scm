#lang racket/base
(require sicp)

(define (make-tree) (cons 'tree '()))
(define (top-node t) (cdr t))
(define (set-top-node! t v) (set-cdr! t v))

(define (make-node left right key value) (cons (cons left right) (cons key value)))
(define (left-node n) (caar n))
(define (right-node n) (cdar n))
(define (key-node n) (cadr n))
(define (value-node n) (cddr n))
(define (set-left-node! n v) (set-car! (car n) v))
(define (set-right-node! n v) (set-cdr! (car n) v))
(define (set-key-node! n v) (set-car! (cdr n) v))
(define (set-value-node! n v) (set-cdr! (cdr n) v))

(define 
  (assoc key node)
  (let 
    ( ;
     (current-key (key-node node))
    )

    (cond 
      ((null? node) #f)
      ((equal? key current-key) node)
      (else
       (if (< key current-key) 
         (assoc key (left-node node))
         (assoc key (right-node node))
       )
      )
    )
  )
)
(define 
  (lookup key tree)
  (let 
    ( ;
     (node (assoc key (cdr tree)))
    )

    (if node 
      (value-node node)
      #f
    )
  )
)

(define 
  (insert! key value tree)

  (define 
    (search-set node)
    (let 
      ( ;
       (current-key (key-node node))
      )

      (cond 
        ((equal? key current-key) (set-value-node! node value))
        (else
         (if (< key current-key) 
           (if (null? (left-node node)) 
             (set-left-node! node (make-node '() '() key value))
             (search-set (left-node node))
           )
           (if (null? (right-node node)) 
             (set-right-node! node (make-node '() '() key value))
             (search-set (right-node node))
           )
         )
        )
      )
    )
  )

  (if (null? (top-node tree)) 
    (set-top-node! tree (make-node '() '() key value))
    (search-set (top-node tree))
  )
  'ok
)

(define tree (make-tree))
(insert! 6 'a tree)
(insert! 3 'b tree)
(insert! 8 'c tree)
(insert! 2 'd tree)
(insert! 4 'e tree)
(insert! 1 'f tree)
(insert! 5 'g tree)
(insert! 7 'h tree)
(insert! 9 'i tree)

(lookup 1 tree)
; 'f
(lookup 2 tree)
; 'd
(lookup 3 tree)
; 'b
(lookup 4 tree)
; 'e
(lookup 5 tree)
; 'g
(lookup 6 tree)
; 'a
(lookup 7 tree)
; 'h
(lookup 8 tree)
; 'c
(lookup 9 tree)
; 'i

(insert! 1 'a tree)
(insert! 2 'b tree)
(insert! 3 'c tree)
(insert! 4 'd tree)
(insert! 5 'e tree)
(insert! 6 'f tree)
(insert! 7 'g tree)
(insert! 8 'h tree)
(insert! 9 'i tree)

(lookup 1 tree)
; 'a
(lookup 2 tree)
; 'b
(lookup 3 tree)
; 'c
(lookup 4 tree)
; 'd
(lookup 5 tree)
; 'e
(lookup 6 tree)
; 'f
(lookup 7 tree)
; 'g
(lookup 8 tree)
; 'h
(lookup 9 tree)
; 'i

; ex2.66と比較して、lookupの処理に共通点があることがわかる。
