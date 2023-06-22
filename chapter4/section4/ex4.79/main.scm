#lang racket
(provide (all-defined-out))

(require sicp)
(require "q-eval/repl.scm")
(require "sample-db.scm")

(define repl (make-repl))
(insert-sample-data repl)

; # 問題文の解釈
; この問題では、変数をリネームする代わりに環境を導入することが求められている。
; 「ブロック構造⼿続きの規則版」とは、Schemeにおける内部手続きのように、環境ごとに規則やアサーションを定義できる機能である。
; クエリ言語にブロック構造が導入されることで、グローバルにすべてのアサーションや規則を定義する必要がなくなり、大規模システムの開発に対してより実用的になる。
; 問題文の最後にある自由回答の問については、題意をいまいち理解できていない。これは自由回答であるためスキップする。
; 博士号をとりたくなったらまた考えることにする。

; # 実装
; 変数のリネームではなく環境を使うようにクエリ言語を実装した。
; 環境は入れ子構造になっており、それぞれの環境内で変数のシンボルは一意だが、全体としては一意でない。
; そのため、変数を一意に特定するために、内部表現を(環境id . シンボル)とした。環境idとは、各環境に付与される一意な値である。
; ほかにも、環境の集合をWorldと呼び、これをフレームの代わりにストリームで持ち回ることにした。
; 更に詳しい説明は、q-eval/environment.scmでコメントしている。

; 変数名の衝突が起きうるのは、規則を適用する部分である。
; オリジナルの実装では、ここで変数のリネームをしていたが、本実装では環境を継承（extend）するようにした。
; 規則の適用は以下のようなステップで実行される：
; 1. 環境を継承して新しい環境を作る。ルールの変数を新しい環境に対応させる
; 2. ユニフィケーションを実行する。この処理はオリジナルの実装とほとんど同じである。変数の束縛は、その変数の環境に作成される。
; 3. 新しい環境を現環境としてqevalによる評価を行い、その結果の現環境をもとの（クエリの）環境にもどす
; 具体的な実装はq-eval/eval-apply.scmにある

; assert!は、現環境にアサーションまたは規則を追加する特殊規則として実装した。
; そのため、assert!はトップレベルか、and内の節として使用することになる。
; orやnotの内部でも使用できるが、これらはその後の環境を使って何かをするわけではないため意味がないからである。

; # サンプルコード
; 以下では従来の記法のみを使用しており、本実装の特徴的な機能は使用していない。
; 従来のコードは同じようにそのまま動作する。
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
(repl '(append-to-form (a b) (c d) ?z))
; {append-to-form {a b} {c d} {a b c d}}
(repl '(append-to-form (a b) ?y (a b c d)))
; {append-to-form {a b} {c d} {a b c d}}
(repl '(append-to-form ?x ?y (a b c d)))
; {append-to-form {a b c d} () {a b c d}}
; {append-to-form () {a b c d} {a b c d}}
; {append-to-form {a} {b c d} {a b c d}}
; {append-to-form {a b} {c d} {a b c d}}
; {append-to-form {a b c} {d} {a b c d}}

; 以下はブロック構造を使用した簡単な例である。
(repl 
  '
  (and 
    ; 以下のように、ローカルの規則やアサーションを定義できる
    (assert! 
      (rule 
        (local-rule ?x)
      )
    )
    (assert! 
      (local-assertion hoge)
    )

    ; ローカルのアサーションや規則は、同じ、またはより深いブロックで利用可能である
    (local-rule 123)
    (local-assertion hoge)
  )
)
; {and {local-rule 123} {local-assertion hoge}}

; しかし、ブロック外からは利用できない
(repl '(local-rule 123))
; 何も表示されない
(repl '(local-assertion hoge))
; 何も表示されない

; 以下はより複雑な例である
(repl 
  '
  (assert! 
    (rule 
      (same ?x ?x)
    )
  )
)
(repl 
  '(and
    ; bigshotをローカル規則として定義する
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
              ; sameはグローバル環境にあるため利用できる
              (not (same ?division ?boss-division))
            )
          )
        )
      )
    )

    (bigshot ?x)
   )
)
; {and {bigshot {Warbucks Oliver}}}
; {and {bigshot {Scrooge Eben}}}
; {and {bigshot {Bitdiddle Ben}}}

; 以下は更に複雑な例であり、assert!の2重の入れ子構造がある
(repl 
  '
  (and 
    ; replaceをローカル規則として定義する
    (assert! 
      (rule 
        (replace ?person1 ?person2)
        (and 
          ; can-do-job-recはreplaceだけで使用する規則であるため、replaceのローカル規則として定義する
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

          (job ?person1 ?job1)
          (job ?person2 ?job2)
          (can-do-job-rec ?job1 ?job2)
          (not (same ?person1 ?person2))
        )
      )
    )

    (replace ?person1 ?person2)
    (salary ?person1 ?salary1)
    (salary ?person2 ?salary2)
    (lisp-value < ?salary1 ?salary2)
  )
)
; {and {replace {Aull DeWitt} {Warbucks Oliver}} {salary {Aull DeWitt} 25000} {salary {Warbucks Oliver} 150000} {lisp-value < 25000 150000}}
; {and {replace {Fect Cy D} {Hacker Alyssa P}} {salary {Fect Cy D} 35000} {salary {Hacker Alyssa P} 40000} {lisp-value < 35000 40000}}
