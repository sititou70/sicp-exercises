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
      (wheel ?person)
      (and 
        (supervisor ?middle-manager ?person)
        (supervisor ?x ?middle-manager)
      )
    )
  )
)

; 以下のクエリの結果、Oliver Warbucksは4回表示される。
(repl '(wheel ?who))
; {wheel {Warbucks Oliver}}
; {wheel {Warbucks Oliver}}
; {wheel {Bitdiddle Ben}}
; {wheel {Warbucks Oliver}}
; {wheel {Warbucks Oliver}}

; これは、Oliver Warbucksに監督される?middle-managerに監督されるものが4人いるためである
(repl 
  '
  (and 
    (supervisor ?middle-manager ?person)
    (supervisor ?x ?middle-manager)
  )
)
; {and {supervisor {Scrooge Eben} {Warbucks Oliver}} {supervisor {Cratchet Robert} {Scrooge Eben}}}
; {and {supervisor {Bitdiddle Ben} {Warbucks Oliver}} {supervisor {Tweakit Lem E} {Bitdiddle Ben}}}
; {and {supervisor {Hacker Alyssa P} {Bitdiddle Ben}} {supervisor {Reasoner Louis} {Hacker Alyssa P}}}
; {and {supervisor {Bitdiddle Ben} {Warbucks Oliver}} {supervisor {Fect Cy D} {Bitdiddle Ben}}}
; {and {supervisor {Bitdiddle Ben} {Warbucks Oliver}} {supervisor {Hacker Alyssa P} {Bitdiddle Ben}}}
