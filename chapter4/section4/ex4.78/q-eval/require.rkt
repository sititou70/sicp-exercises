#lang racket
(provide (all-defined-out))

(require sicp)

(define 
  (install-require repl)

  (repl 
    '( ;
      (define 
        (require p)
        (if (not p) (amb))
      )
     )
  )
)
