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
(repl 
  '
  (assert! 
    (rule 
      (lives-near ?person-1 ?person-2)
      (and 
        (address ?person-1 (?town . ?rest-1))
        (address ?person-2 (?town . ?rest-2))
        (not (same ?person-1 ?person-2))
      )
    )
  )
)

(repl '(lives-near ?person-1 ?person-2))
; {lives-near {Aull DeWitt} {Reasoner Louis}}
; {lives-near {Aull DeWitt} {Bitdiddle Ben}}
; {lives-near {Reasoner Louis} {Aull DeWitt}}
; {lives-near {Reasoner Louis} {Bitdiddle Ben}}
; {lives-near {Hacker Alyssa P} {Fect Cy D}}
; {lives-near {Fect Cy D} {Hacker Alyssa P}}
; {lives-near {Bitdiddle Ben} {Aull DeWitt}}
; {lives-near {Bitdiddle Ben} {Reasoner Louis}}

; 互いに近所に住んでいる人のペアが2回表示されてしまっている。lives-nearは可換な関係だからである。
; 条件を加えることによって関係を可換でなくすることで、ペアが1度だけ表示されるようにできる。

(repl 
  '
  (assert! 
    (rule 
      (lives-near-uniq ?person-1 ?person-2)
      (and 
        (address ?person-1 (?town . ?rest-1))
        (address ?person-2 (?town . ?rest-2))
        (not (same ?person-1 ?person-2))
        (lisp-value 
          (lambda (person-1 person-2) 
            (define o1 (open-output-string))
            (write person-1 o1)
            (define str1 (get-output-string o1))

            (define o2 (open-output-string))
            (write person-2 o2)
            (define str2 (get-output-string o2))

            (string<? str1 str2)
          )
          ?person-1
          ?person-2
        )
      )
    )
  )
)
(repl '(lives-near-uniq ?person-1 ?person-2))
; {lives-near-uniq {Aull DeWitt} {Reasoner Louis}}
; {lives-near-uniq {Aull DeWitt} {Bitdiddle Ben}}
; {lives-near-uniq {Fect Cy D} {Hacker Alyssa P}}
; {lives-near-uniq {Bitdiddle Ben} {Reasoner Louis}}
