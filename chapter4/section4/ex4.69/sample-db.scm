#lang racket
(provide (all-defined-out))

(require sicp)
(require "q-eval/repl.scm")

(define 
  (insert-sample-data)

  (define repl (make-repl 'silent))

  (repl '(assert! (son Adam Cain)))
  (repl '(assert! (son Cain Enoch)))
  (repl '(assert! (son Enoch Irad)))
  (repl '(assert! (son Irad Mehujael)))
  (repl '(assert! (son Mehujael Methushael)))
  (repl '(assert! (son Methushael Lamech)))
  (repl '(assert! (wife Lamech Ada)))
  (repl '(assert! (son Ada Jabal)))
  (repl '(assert! (son Ada Jubal)))
)
