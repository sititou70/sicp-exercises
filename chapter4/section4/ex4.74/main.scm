#lang racket
(provide (all-defined-out))

(require sicp)
(require "q-eval/repl.scm")
(require "sample-db.scm")

(define repl (make-repl 'display))
(insert-sample-data)

; main
; a. 回答はq-eval/stream.scmに示した

; b. クエリシステムのふるまいに変化はない。
; 問題文中にもあるとおり、negate、lisp-value、find-assertionsがstream-flatmapに入力するstreamの各要素は、空ストリームか単一要素のストリームである。
; より具体的には、streamは((1) (()) (2) (()))のようなものになる。

; したがって、interleave-delayedは単純にそれぞれのstreamの各要素をappendしたような動作になる。
; より具体的には、(1)と(flatten-streamによる残りの処理)をinterleaveすることになる。
; しかし(1)は要素が1つしかないため、あとのストリームは(flatten-streamによる残りの処理)とするしかない。

; 結局、これはAlyssaが提案したプログラムと同じ動作になる。

; 以下は動作テストである
(repl 
  '(and
    (not (lisp-valie (lambda (x) #f) 1))
    (supervisor ?person (Bitdiddle Ben))
   )
)
; {and {not {lisp-valie {lambda {x} #f} 1}} {supervisor {Tweakit Lem E} {Bitdiddle Ben}}}
; {and {not {lisp-valie {lambda {x} #f} 1}} {supervisor {Fect Cy D} {Bitdiddle Ben}}}
; {and {not {lisp-valie {lambda {x} #f} 1}} {supervisor {Hacker Alyssa P} {Bitdiddle Ben}}}
