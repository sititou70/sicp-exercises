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

(repl 
  '
  (assert! 
    (rule 
      (replace ?person1 ?person2)
      (and 
        (job ?person1 ?job1)
        (job ?person2 ?job2)
        (can-do-job-rec ?job1 ?job2)
        (not (same ?person1 ?person2))
      )
    )
  )
)

(repl '(replace ?x (Fect Cy D)))
; {replace {Hacker Alyssa P} {Fect Cy D}}
; {replace {Bitdiddle Ben} {Fect Cy D}}

(repl 
  '
  (and 
    (replace ?person1 ?person2)
    (salary ?person1 ?salary1)
    (salary ?person2 ?salary2)
    (lisp-value < ?salary1 ?salary2)
  )
)
; {and {replace {Aull DeWitt} {Warbucks Oliver}} {salary {Aull DeWitt} 25000} {salary {Warbucks Oliver} 150000} {lisp-value < 25000 150000}}
; {and {replace {Fect Cy D} {Hacker Alyssa P}} {salary {Fect Cy D} 35000} {salary {Hacker Alyssa P} 40000} {lisp-value < 35000 40000}}
