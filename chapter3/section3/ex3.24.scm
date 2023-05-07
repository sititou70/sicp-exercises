#lang sicp

(define 
  (make-table)
  (list '*table*)
)
(define 
  (assoc key records)

  (define 
    (same-key? a b)
    (cond 
      ((and (number? a) (number? b))
       (< (abs (- a b)) 0.0001)
      )
      (else (equal? a b))
    )
  )

  (cond 
    ((null? records) false)
    ((same-key? key (caar records)) (car records))
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

(define t (make-table))
(insert! 0.1 'a t)
(insert! 0.2 'b t)
(insert! (+ 0.1 0.2) 'c t)

(lookup 0.3 t)
; c
