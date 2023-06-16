#lang racket
(provide (all-defined-out))

(require sicp)
(require "q-eval/repl.scm")

(define repl (make-repl 'display))

; main
(repl 
  '
  (assert! 
    (rule 
      (append-to-form () ?y ?y)
    )
  )
)
(repl 
  '
  (assert! 
    (rule 
      (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z)
    )
  )
)
(repl 
  '
  (assert! (rule (reverse1 () ())))
)
(repl 
  '
  (assert! 
    (rule 
      (reverse1 (?x . ?xr) ?rev)
      (and 
        (reverse1 ?xr ?xr-rev)
        (append-to-form ?xr-rev (?x) ?rev)
      )
    )
  )
)

(repl '(reverse1 (1 2 3) ?x))
; {reverse {1 2 3} {3 2 1}}

; ?xrが定数にバインドされないため、再帰呼出しが終わらず無限ループになる
; (repl '(reverse1 ?x (1 2 3)))
