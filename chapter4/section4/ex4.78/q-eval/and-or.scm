#lang racket
(provide (all-defined-out))

(require sicp)

(define 
  (install-and-or repl)

  (repl 
    '( ;
      (define 
        (and p1 p2)
        (if p1 p2 false)
      )
      (define 
        (or p1 p2)
        (if p1 p1 p2)
      )
     )
  )
)
