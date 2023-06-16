#lang racket
(provide (all-defined-out))

(require sicp)
(require "q-eval/repl.scm")
(require "sample-db.scm")

(define repl (make-repl 'display))
(insert-sample-data)

; main
; frameに、最後に適用した規則の情報も持たせるようにした。
; そして規則を適用するごとに、直前に適用された規則を再度適用しないようにチェックするようにした。
(repl 
  '
  (assert! 
    (rule 
      (outranked-by ?staff-person ?boss)
      (or 
        (supervisor ?staff-person ?boss)
        (and 
          (supervisor ?staff-person ?middle-manager)
          (outranked-by ?middle-manager ?boss)
        )
      )
    )
  )
)

(repl 
  '
  (assert! 
    (rule 
      (louis-outranked-by ?staff-person ?boss)
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

; 通常の規則の適用は今までどおり行える
(repl '(outranked-by ?who (Bitdiddle Ben)))
; {outranked-by {Tweakit Lem E} {Bitdiddle Ben}}
; {outranked-by {Fect Cy D} {Bitdiddle Ben}}
; {outranked-by {Hacker Alyssa P} {Bitdiddle Ben}}

(repl '(outranked-by (Bitdiddle Ben) ?who))
; {outranked-by {Bitdiddle Ben} {Warbucks Oliver}}

; また、無限ループを検知すると停止するようになった
(repl '(louis-outranked-by (Bitdiddle Ben) ?who))
; {louis-outranked-by {Bitdiddle Ben} {Warbucks Oliver}}

; しかし、この機構は完璧ではない。例えば、以下のような相互再帰の規則の無限ループは防げない
(repl 
  '
  (assert! 
    (rule 
      (a ?x)
      (b ?x)
    )
  )
)
(repl 
  '
  (assert! 
    (rule 
      (b ?x)
      (a ?x)
    )
  )
)

; 停止しない
(repl '(a ?x))
