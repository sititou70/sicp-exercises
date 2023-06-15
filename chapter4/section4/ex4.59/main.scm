#lang racket
(provide (all-defined-out))

(require sicp)
(require "q-eval/repl.scm")
(require "sample-db.scm")

(define repl (make-repl 'display))
(insert-sample-data)

; main
; a
(repl '(meeting ?division (Friday . ?rest)))
; {meeting administration {Friday 1pm}}

; b
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
      (meeting-time ?person ?day-and-time)
      (and 
        (job ?person (?division . ?rest))
        (meeting ?meeting-division ?day-and-time)
        (or 
          (same ?meeting-division ?division)
          (same ?meeting-division whole-company)
        )
      )
    )
  )
)

; c
(repl '(meeting-time (Hacker Alyssa P) (Wednesday . ?rest)))
; {meeting-time {Hacker Alyssa P} {Wednesday 3pm}}
; {meeting-time {Hacker Alyssa P} {Wednesday 4pm}}
