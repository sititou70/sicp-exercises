#lang racket
(provide (all-defined-out))

(require sicp)
(require "q-eval/repl.rkt")
(require "sample-db.rkt")

(define repl (make-repl 'display))
(insert-sample-data)

; main
(repl 
  '
  (assert! 
    (rule 
      (same ?x ?x)
    )
  )
)

(repl 
  '
  (assert! 
    (rule 
      (bigshot ?person)
      (and 
        (job ?person (?division . ?rest1))
        (or 
          (not (supervisor ?person ?boss))
          (and 
            (supervisor ?person ?boss)
            (job ?boss (?boss-division . ?rest2))
            (not (same ?division ?boss-division))
          )
        )
      )
    )
  )
)

(repl '(bigshot ?x))
; {bigshot {Warbucks Oliver}}
; {bigshot {Scrooge Eben}}
; {bigshot {Bitdiddle Ben}}