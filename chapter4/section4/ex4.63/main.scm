#lang racket
(provide (all-defined-out))

(require sicp)
(require "q-eval/repl.scm")
(require "sample-db.scm")

(define repl (make-repl 'display))
(insert-sample-data)

; main
(repl 
  '
  (assert! 
    (rule 
      (grandson ?g ?s)
      (and 
        (son ?g ?f)
        (son ?f ?s)
      )
    )
  )
)

(repl 
  '
  (assert! 
    (rule 
      (son ?m ?s)
      (and 
        (wife ?m ?w)
        (son ?w ?s)
      )
    )
  )
)

(repl '(grandson Cain ?x))
(repl '(son Lamech ?x))
(repl '(grandson Methushael ?x))
