#lang racket/base

(define 
  (accumulate op initial sequence)
  (if (null? sequence) 
    initial
    (op 
      (car sequence)
      (accumulate op initial (cdr sequence))
    )
  )
)

(define 
  (map p sequence)
  (accumulate 
    (lambda (x y) 
      (cons 
        (p x)
        y
      )
    )
    null
    sequence
  )
)

(define 
  (accumulate-n op init seqs)
  (if (null? (car seqs)) 
    null
    (cons 
      (accumulate 
        op
        init
        (map car seqs)
      )
      (accumulate-n 
        op
        init
        (map cdr seqs)
      )
    )
  )
)

(define 
  s
  (list 
    (list 1 2 3)
    (list 4 5 6)
    (list 7 8 9)
    (list 10 11 12)
  )
)
(accumulate-n + 0 s)
; (22 26 30)
