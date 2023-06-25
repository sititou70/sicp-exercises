#lang racket/base
(require sicp)
(require "../../sicp-env-utils.rkt")

(define 
  (make-table)
  (list '*table*)
)
(define 
  (assoc keys records)

  (define 
    (same-key? keys1 keys2)
    (if (= (length keys1) (length keys2)) 
      (fold (lambda (x y) (and x y)) #t (map equal? keys1 keys2))
      #f
    )
  )

  (cond 
    ((null? records) #f)
    ((same-key? keys (caar records)) (car records))
    (else (assoc keys (cdr records)))
  )
)
(define 
  (lookup table . keys)
  (let 
    ( ;
     (record (assoc keys (cdr table)))
    )

    (if record 
      (cdr record)
      #f
    )
  )
)
(define 
  (insert! table value . keys)
  (let 
    ( ;
     (record (assoc keys (cdr table)))
    )

    (if record 
      (set-cdr! record value)
      (set-cdr! 
        table
        (cons (cons keys value) 
              (cdr table)
        )
      )
    )
  )
  'ok
)

(define t (make-table))
(insert! t 1 'a)
(insert! t 2 'a 'b)
(insert! t 3 'a 'c)
(insert! t 4 'a 'b 'd)
(insert! t 5 'a 'b 'd 'e)

(lookup t 'a)
(lookup t 'a 'b)
(lookup t 'a 'c)
(lookup t 'a 'b 'd)
(lookup t 'a 'b 'd 'e)
