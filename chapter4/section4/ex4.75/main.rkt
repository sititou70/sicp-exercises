#lang racket
(provide (all-defined-out))

(require sicp)
(require "q-eval/repl.rkt")
(require "sample-db.rkt")

(define repl (make-repl 'display))
(insert-sample-data)

; main
; 仕事が(computer wizard)である一意な人物
(repl '(unique (job ?x (computer wizard))))
; {unique {job {Bitdiddle Ben} {computer wizard}}}

; 仕事が(computer programmer)である一意な人物（存在しない）
(repl '(unique (job ?x (computer programmer))))
; 空のストリーム

; 自分の他に同じ仕事をしている人がいない人物
(repl 
  '
  (and 
    (job ?x ?j)
    (unique (job ?anyone ?j))
  )
)
; {and {job {Aull DeWitt} {administration secretary}} {unique {job {Aull DeWitt} {administration secretary}}}}
; {and {job {Cratchet Robert} {accounting scrivener}} {unique {job {Cratchet Robert} {accounting scrivener}}}}
; {and {job {Scrooge Eben} {accounting chief accountant}} {unique {job {Scrooge Eben} {accounting chief accountant}}}}
; {and {job {Warbucks Oliver} {administration big wheel}} {unique {job {Warbucks Oliver} {administration big wheel}}}}
; {and {job {Reasoner Louis} {computer programmer trainee}} {unique {job {Reasoner Louis} {computer programmer trainee}}}}
; {and {job {Tweakit Lem E} {computer technician}} {unique {job {Tweakit Lem E} {computer technician}}}}
; {and {job {Bitdiddle Ben} {computer wizard}} {unique {job {Bitdiddle Ben} {computer wizard}}}}

; ちょうど1人の部下を持つ人物
(repl 
  '
  (and 
    (job ?person ?job)
    (unique (supervisor ?member ?person))
  )
)
; {and {job {Scrooge Eben} {accounting chief accountant}} {unique {supervisor {Cratchet Robert} {Scrooge Eben}}}}
; {and {job {Hacker Alyssa P} {computer programmer}} {unique {supervisor {Reasoner Louis} {Hacker Alyssa P}}}}
