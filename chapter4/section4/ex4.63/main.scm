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
; {grandson Cain Irad}

(repl '(son Lamech ?x))
; {son Lamech Jubal}
; {son Lamech Jabal}

(repl '(grandson Methushael ?x))
; {grandson Methushael Jubal}
; {grandson Methushael Jabal}
