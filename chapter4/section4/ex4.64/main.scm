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
      (outranked-by ?staff-person ?boss)
      (or 
        (supervisor ?staff-person ?boss)
        (and 
          (outranked-by ?middle-manager ?boss)
          (supervisor ?staff-person ?middle-manager)
        )
      )
    )
  )
)

; 以下のクエリは停止しない。outranked-byの再帰呼出しは、andの先頭にあるからである。
; 再帰呼出しに与えられるフレームストリームは全くフィルタされていないため停止しない。
(repl '(outranked-by (Bitdiddle Ben) ?who))
