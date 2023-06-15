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
      (same ?x ?x)
    )
  )
)

; a
(repl '(meeting ?division (Friday . ?rest)))
; {meeting administration {Friday 1pm}}

; b
(repl 
  '
  (assert! 
    (rule 
      (meeting-time ?person ?day-and-time)
    )
  )
)
        )
      )
    )
  )
)

(repl '(bigshot ?x))
