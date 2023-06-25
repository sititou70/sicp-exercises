#lang racket
(provide (all-defined-out))

(require sicp)
(require "q-eval/repl.rkt")

(define repl (make-repl 'display))

; main
(repl 
  '
  (assert! 
    (rule 
      ; ?xと?yがリストの先頭に見つかって、残りの要素である
      (?x next-to ?y in (?x ?y . ?u))
    )
  )
)
; または
(repl 
  '
  (assert! 
    (rule 
      ; ?xがリストの先頭に見つからず、残りの要素がある場合は
      (?x next-to ?y in (?v . ?z))
      ; 再帰的に残りの要素についても規則が適用される
      (?x next-to ?y in ?z)
    )
  )
)

(repl '(?x next-to ?y in (1 (2 3) 4)))
; {{2 3} next-to 4 in {1 {2 3} 4}}
; {1 next-to {2 3} in {1 {2 3} 4}}

(repl '(?x next-to 1 in (2 1 3 1)))
; {3 next-to 1 in {2 1 3 1}}
; {2 next-to 1 in {2 1 3 1}}
