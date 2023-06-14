#lang racket
(provide (all-defined-out))

(require sicp)
(require "q-eval/repl.scm")
(require "sample-db.scm")

(define repl (make-repl 'display))
(insert-sample-data)

; main
(repl '(supervisor ?person (Bitdiddle Ben)))
; {supervisor {Tweakit Lem E} {Bitdiddle Ben}}
; {supervisor {Fect Cy D} {Bitdiddle Ben}}
; {supervisor {Hacker Alyssa P} {Bitdiddle Ben}}

(repl '(job ?person (accounting . ?rest)))
; {job {Cratchet Robert} {accounting scrivener}}
; {job {Scrooge Eben} {accounting chief accountant}}

(repl '(address ?person (Slumerville . ?rest)))
; {address {Aull DeWitt} {Slumerville {Onion Square} 5}}
; {address {Reasoner Louis} {Slumerville {Pine Tree Road} 80}}
; {address {Bitdiddle Ben} {Slumerville {Ridge Road} 10}}
