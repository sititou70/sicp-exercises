#lang racket/base
(require sicp)

; utils
;; see: https://github.com/racket/srfi/blob/25eb1c0e1ab8a1fa227750aa7f0689a2c531f8c8/srfi-lite-lib/srfi/1/predicate.rkt#LL86C1-L89C64
(define 
  (null-list? l)
  (cond 
    ((pair? l) #f)
    ((null? l) #t)
    (else (error "null-list?: argument out of domain" l))
  )
)

;; see: https://github.com/racket/srfi/blob/25eb1c0e1ab8a1fa227750aa7f0689a2c531f8c8/srfi-lite-lib/srfi/1/fold.rkt#LL77C1-L86C50
(define 
  (fold kons knil lis1 . lists)
  (let 
    lp
    ((lis lis1) (ans knil)) ; Fast path
    (if (null-list? lis) 
      ans
      (lp (cdr lis) (kons (car lis) ans))
    )
  )
)

; table
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
