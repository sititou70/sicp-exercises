#lang racket/base

(define 
  (same-parity first-item . items)

  (define first-item-remainder (remainder first-item 2))

  (define 
    (filter items)
    (cond 
      ((null? items) null)
      ((= (remainder (car items) 2) first-item-remainder)
       (cons (car items) (filter (cdr items)))
      )
      (else (filter (cdr items)))
    )
  )

  (cons first-item (filter items))
)

(same-parity 1 2 3 4 5 6 7)
; '(1 3 5 7)
(same-parity 2 3 4 5 6 7)
; '(2 4 6)