#lang racket
(provide (all-defined-out))

(require sicp)
(require "q-eval/repl.rkt")
(require "sample-db.rkt")

(define repl (make-repl 'display))
(insert-sample-data)

; main
(define 
  (driver-loop)
  (display "[q-eval] > ")
  (repl (read))
  (driver-loop)
)
(driver-loop)
