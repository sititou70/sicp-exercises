#lang racket
(provide (all-defined-out))

(require sicp)
(require "q-eval/repl.rkt")
(require "sample-db.rkt")

(define repl (make-repl 'display))
(insert-sample-data)

; main
; notとlisp-valueについて、それらのqeval時に、そのクエリの実行に十分な束縛があればフィルタを実行し、なければ実行を遅らせるようにした。
; frameの構造を変更し、将来実行すべきフィルタの列を保持するようにした。
; フィルタの実行に十分が束縛がないときは、この列にフィルタのタグと本体が追加される。
; 追加されたフィルタは、andの各節を処理するたびに実行可能化検査され、可能であれば実行される。
; 検査はandの各節で行うだけで十分である。なぜなら、frameストリームを「直列」に処理する構文はいまのところandしかなく、
; 束縛が増える可能性があるのはこの部分だけだからである。

; 以下の2つのクエリは、オリジナルの環境では後者が空ストリームになってしまっていたが、本環境では同じ結果を返す
(repl 
  '
  (and 
    (supervisor ?x ?y)
    (not (job ?x (computer programmer)))
  )
)
; {and {supervisor {Aull DeWitt} {Warbucks Oliver}} {not {job {Aull DeWitt} {computer programmer}}}}
; {and {supervisor {Cratchet Robert} {Scrooge Eben}} {not {job {Cratchet Robert} {computer programmer}}}}
; {and {supervisor {Scrooge Eben} {Warbucks Oliver}} {not {job {Scrooge Eben} {computer programmer}}}}
; {and {supervisor {Bitdiddle Ben} {Warbucks Oliver}} {not {job {Bitdiddle Ben} {computer programmer}}}}
; {and {supervisor {Reasoner Louis} {Hacker Alyssa P}} {not {job {Reasoner Louis} {computer programmer}}}}
; {and {supervisor {Tweakit Lem E} {Bitdiddle Ben}} {not {job {Tweakit Lem E} {computer programmer}}}}
(repl 
  '
  (and 
    (not (job ?x (computer programmer)))
    (supervisor ?x ?y)
  )
)
; {and {not {job {Aull DeWitt} {computer programmer}}} {supervisor {Aull DeWitt} {Warbucks Oliver}}}
; {and {not {job {Cratchet Robert} {computer programmer}}} {supervisor {Cratchet Robert} {Scrooge Eben}}}
; {and {not {job {Scrooge Eben} {computer programmer}}} {supervisor {Scrooge Eben} {Warbucks Oliver}}}
; {and {not {job {Bitdiddle Ben} {computer programmer}}} {supervisor {Bitdiddle Ben} {Warbucks Oliver}}}
; {and {not {job {Reasoner Louis} {computer programmer}}} {supervisor {Reasoner Louis} {Hacker Alyssa P}}}
; {and {not {job {Tweakit Lem E} {computer programmer}}} {supervisor {Tweakit Lem E} {Bitdiddle Ben}}}


; 以下の2つのクエリも同様である。後者はオリジナルの環境ではエラーとなっていた
(repl 
  '
  (and 
    (salary ?person ?amount)
    (lisp-value > ?amount 30000)
  )
)
; {and {salary {Scrooge Eben} 75000} {lisp-value > 75000 30000}}
; {and {salary {Warbucks Oliver} 150000} {lisp-value > 150000 30000}}
; {and {salary {Fect Cy D} 35000} {lisp-value > 35000 30000}}
; {and {salary {Hacker Alyssa P} 40000} {lisp-value > 40000 30000}}
; {and {salary {Bitdiddle Ben} 60000} {lisp-value > 60000 30000}}
(repl 
  '
  (and 
    (lisp-value > ?amount 30000)
    (salary ?person ?amount)
  )
)
; {and {lisp-value > 75000 30000} {salary {Scrooge Eben} 75000}}
; {and {lisp-value > 150000 30000} {salary {Warbucks Oliver} 150000}}
; {and {lisp-value > 35000 30000} {salary {Fect Cy D} 35000}}
; {and {lisp-value > 40000 30000} {salary {Hacker Alyssa P} 40000}}
; {and {lisp-value > 60000 30000} {salary {Bitdiddle Ben} 60000}}

; 本環境では、将来実行すべきフィルタが最後まで残ってしまう場合がある。
; その場合フィルタは実行されず、ユーザーがクエリの構築を誤っている可能性が高い。
; 望むなら、フィルタが残っている場合replを変更して警告を表示することもできる。
; しかし、そのようなフィルタは変数が「?」として表示され、不具合があることがわかりやすいため、特別な警告処理は実装しなかった。
(repl 
  '
  (and 
    (lisp-value > ?hoge 30000)
    (salary ?person ?amount)
  )
)
; {and {lisp-value > ?hoge 30000} {salary {Aull DeWitt} 25000}}
; {and {lisp-value > ?hoge 30000} {salary {Cratchet Robert} 18000}}
; {and {lisp-value > ?hoge 30000} {salary {Scrooge Eben} 75000}}
; {and {lisp-value > ?hoge 30000} {salary {Warbucks Oliver} 150000}}
; {and {lisp-value > ?hoge 30000} {salary {Reasoner Louis} 30000}}
; {and {lisp-value > ?hoge 30000} {salary {Tweakit Lem E} 25000}}
; {and {lisp-value > ?hoge 30000} {salary {Fect Cy D} 35000}}
; {and {lisp-value > ?hoge 30000} {salary {Hacker Alyssa P} 40000}}
; {and {lisp-value > ?hoge 30000} {salary {Bitdiddle Ben} 60000}}
