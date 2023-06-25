#lang racket

(require sicp)
(require "make-repl.rkt")
(require "sample-db.rkt")

(define repl (make-repl 'stdout))
(define repl-silent (make-repl 'silent))
(insert-sample-data)

; main
; 4.3節の非決定性計算の評価器の上に論理プログラミングの評価器を構築した

; m-evalディレクトリは4.3節の非決定性計算の評価器である。
; ただし、try-catchという新しい構文を組み込んである。論理プログラミングのnotを実装するために必要だったからである。
; try-catchはif-failに似ているが、最初の式が1度でも成功するとfail句の式は評価されないところが違う。
; notの実装にif-failを用いると、最終的にはfail句が評価されてnotは常に真になってしまうからである。

; q-evalディレクトリは4.4節の論理プログラミングの評価器である。
; 全体的に、フレームの「可能性」をストリームとしてではなく、ambによる曖昧な選択によって実現している。

; インデックスによるパフォーマンス最適化の実装は行わなかった。
; 基底Lisp環境のハッシュマップを非決定性計算に組み込むことで実装することもできたが、
; 非決定性計算による復元に対応しないまま組み込むのはすこし気持ちが悪いということと、
; パフォーマンスの遅さが問題になってから実装するほうが良いという決定によるものである。

; オリジナルの論理プログラミング評価器と本実装の違いは、ストリームの挙動にある。
; 例えば、オリジナルの実装ではストリームを互い違いにマージしていたが、本実装では行っていない。
; そのため、結果の表示順に些細な違いがある。例えばbigshotのクエリの結果に違いが出る。

; オリジナルの結果：
; {bigshot {Warbucks Oliver}}
; {bigshot {Scrooge Eben}}
; {bigshot {Bitdiddle Ben}}

; 本実装の結果：
; {bigshot {Scrooge Eben}}
; {bigshot {Warbucks Oliver}}
; {bigshot {Bitdiddle Ben}}

; これは、bigshot規則内で使用されているorを処理する際、オリジナルの実装ではinterleaveを使用していたが、本実装では使用していないことによる。

; 以下はテストである。

(repl ' '(address ?person (Slumerville . ?rest)))
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
; ;;; Starting a new problem: {repl {quote {address ?person {Slumerville . ?rest}}}}
; {address {Aull DeWitt} {Slumerville {Onion Square} 5}}
; {address {Reasoner Louis} {Slumerville {Pine Tree Road} 80}}
; {address {Bitdiddle Ben} {Slumerville {Ridge Road} 10}}
; ;;; There are no more values of: {repl {quote {address ?person {Slumerville . ?rest}}}}

(repl-silent 
  '
  '
  (assert! 
    (rule 
      (same ?x ?x)
    )
  )
)
(repl-silent 
  '
  '
  (assert! 
    (rule 
      (can-do-job-rec ?job1 ?job2)
      (or 
        (same ?job1 ?job2)
        (and 
          (can-do-job ?job1 ?m)
          (can-do-job-rec ?m ?job2)
        )
      )
    )
  )
)
(repl-silent 
  '
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
(repl 
  '
  '
  (and 
    (replace ?person1 ?person2)
    (salary ?person1 ?salary1)
    (salary ?person2 ?salary2)
    (lisp-value < ?salary1 ?salary2)
  )
)
(repl 'try-again)
(repl 'try-again)
; ;;; Starting a new problem: {repl {quote {and {replace ?person1 ?person2} {salary ?person1 ?salary1} {salary ?person2 ?salary2} {lisp-value < ?salary1 ?salary2}}}}
; {and {replace {Aull DeWitt} {Warbucks Oliver}} {salary {Aull DeWitt} 25000} {salary {Warbucks Oliver} 150000} {lisp-value < 25000 150000}}
; {and {replace {Fect Cy D} {Hacker Alyssa P}} {salary {Fect Cy D} 35000} {salary {Hacker Alyssa P} 40000} {lisp-value < 35000 40000}}
; ;;; There are no more values of: {repl {quote {and {replace ?person1 ?person2} {salary ?person1 ?salary1} {salary ?person2 ?salary2} {lisp-value < ?salary1 ?salary2}}}}

(repl-silent 
  '
  '
  (assert! 
    (rule 
      (bigshot ?person)
      (and 
        (job ?person (?division . ?rest1))
        (or 
          (not (supervisor ?person ?boss))
          (and 
            (supervisor ?person ?boss)
            (job ?boss (?boss-division . ?rest2))
            (not (same ?division ?boss-division))
          )
        )
      )
    )
  )
)
(repl ' '(bigshot ?x))
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
; ;;; Starting a new problem: {repl {quote {bigshot ?x}}}
; {bigshot {Scrooge Eben}}
; {bigshot {Warbucks Oliver}}
; {bigshot {Bitdiddle Ben}}
; ;;; There are no more values of: {repl {quote {bigshot ?x}}}

(repl-silent 
  '
  '
  (assert! 
    (rule 
      (append-to-form () ?y ?y)
    )
  )
)
(repl-silent 
  '
  '
  (assert! 
    (rule 
      (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z)
    )
  )
)
(repl ' '(append-to-form (a b) (c d) ?z))
(repl 'try-again)
; ;;; Starting a new problem: {repl {quote {append-to-form {a b} {c d} ?z}}}
; {append-to-form {a b} {c d} {a b c d}}
; ;;; There are no more values of: {repl {quote {append-to-form {a b} {c d} ?z}}}

(repl ' '(append-to-form (a b) ?y (a b c d)))
(repl 'try-again)
; ;;; Starting a new problem: {repl {quote {append-to-form {a b} ?y {a b c d}}}}
; {append-to-form {a b} {c d} {a b c d}}
; ;;; There are no more values of: {repl {quote {append-to-form {a b} ?y {a b c d}}}}

(repl ' '(append-to-form ?x ?y (a b c d)))
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
; ;;; Starting a new problem: {repl {quote {append-to-form ?x ?y {a b c d}}}}
; {append-to-form {a b c d} () {a b c d}}
; {append-to-form {a b c} {d} {a b c d}}
; {append-to-form {a b} {c d} {a b c d}}
; {append-to-form {a} {b c d} {a b c d}}
; {append-to-form () {a b c d} {a b c d}}
; ;;; There are no more values of: {repl {quote {append-to-form ?x ?y {a b c d}}}}
