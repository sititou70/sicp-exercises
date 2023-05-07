#lang racket/base
(require sicp)

; table
(define 
  (make-table)
  (list '*table*)
)
(define 
  (assoc key records)
  (cond 
    ((null? records) false)
    ((equal? key (caar records)) (car records))
    (else (assoc key (cdr records)))
  )
)
(define 
  (lookup key table)
  (let 
    ((record (assoc key (cdr table))))
    (if record 
      (cdr record)
      false
    )
  )
)
(define 
  (insert! key value table)
  (let 
    ((record (assoc key (cdr table))))
    (if record 
      (set-cdr! record value)
      (set-cdr! 
        table
        (cons (cons key value) 
              (cdr table)
        )
      )
    )
  )
  'ok
)

; tree
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
  (assoc-tree key node)
  (if (null? node) 
    #f
    (let 
      ( ;
       (current-key (key-node node))
      )

      (cond 
        ((equal? key current-key) node)
        (else
         (if (< key current-key) 
           (assoc-tree key (left-node node))
           (assoc-tree key (right-node node))
         )
        )
      )
    )
  )
)
(define 
  (lookup-tree key tree)
  (let 
    ( ;
     (node (assoc-tree key (cdr tree)))
    )

    (if node 
      (value-node node)
      #f
    )
  )
)

(define 
  (insert-tree! key value tree)

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
(insert-tree! 6 'a tree)
(insert-tree! 3 'b tree)
(insert-tree! 8 'c tree)
(insert-tree! 2 'd tree)
(insert-tree! 4 'e tree)
(insert-tree! 1 'f tree)
(insert-tree! 5 'g tree)
(insert-tree! 7 'h tree)
(insert-tree! 9 'i tree)

(lookup-tree 1 tree)
; 'f
(lookup-tree 2 tree)
; 'd
(lookup-tree 3 tree)
; 'b
(lookup-tree 4 tree)
; 'e
(lookup-tree 5 tree)
; 'g
(lookup-tree 6 tree)
; 'a
(lookup-tree 7 tree)
; 'h
(lookup-tree 8 tree)
; 'c
(lookup-tree 9 tree)
; 'i

(insert-tree! 1 'a tree)
(insert-tree! 2 'b tree)
(insert-tree! 3 'c tree)
(insert-tree! 4 'd tree)
(insert-tree! 5 'e tree)
(insert-tree! 6 'f tree)
(insert-tree! 7 'g tree)
(insert-tree! 8 'h tree)
(insert-tree! 9 'i tree)

(lookup-tree 1 tree)
; 'a
(lookup-tree 2 tree)
; 'b
(lookup-tree 3 tree)
; 'c
(lookup-tree 4 tree)
; 'd
(lookup-tree 5 tree)
; 'e
(lookup-tree 6 tree)
; 'f
(lookup-tree 7 tree)
; 'g
(lookup-tree 8 tree)
; 'h
(lookup-tree 9 tree)
; 'i

; ex2.66と比較して、lookupの処理に共通点があるとわかる。

(require (only-in srfi/1 [iota srfi-iota] [for-each srfi-for-each]))

(display "ベンチマーク")
(newline)

(define trial 10000)

(display "table: ")
(define bench-table (make-table))
(define table-start (current-process-milliseconds))
(srfi-for-each 
  (lambda (_) 
    (insert! (random trial) (random trial) bench-table)
  )
  (srfi-iota trial)
)
(srfi-for-each 
  (lambda (_) 
    (lookup (random trial) bench-table)
  )
  (srfi-iota trial)
)
(define table-end (current-process-milliseconds))
(display (- table-end table-start))
(display "msec")
(newline)

(display "tree: ")
(define bench-tree (make-tree))
(define tree-start (current-process-milliseconds))
(srfi-for-each 
  (lambda (_) 
    (insert-tree! (random trial) (random trial) bench-tree)
  )
  (srfi-iota trial)
)
(srfi-for-each 
  (lambda (_) 
    (lookup-tree (random trial) bench-tree)
  )
  (srfi-iota trial)
)
(define tree-end (current-process-milliseconds))
(display (- tree-end tree-start))
(display "msec")
(newline)
