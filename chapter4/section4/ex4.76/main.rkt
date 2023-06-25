#lang racket
(provide (all-defined-out))

(require sicp)
(require "q-eval/repl.rkt")
(require "sample-db.rkt")

(define repl (make-repl 'display))
(insert-sample-data)

; main
; この環境では、問題文で示されたアプローチを実装している。
; 以下に示すクエリは、オリジナルの環境と同じような結果を返す。
(repl 
  '
  (and)
)
; {and}

(repl 
  '
  (and 
    (supervisor ?person (Bitdiddle Ben))
  )
)
; {and {supervisor {Tweakit Lem E} {Bitdiddle Ben}}}
; {and {supervisor {Fect Cy D} {Bitdiddle Ben}}}
; {and {supervisor {Hacker Alyssa P} {Bitdiddle Ben}}}

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
      (meeting-time ?person ?day-and-time)
      (and 
        (job ?person (?division . ?rest))
        (meeting ?meeting-division ?day-and-time)
        (or 
          (same ?meeting-division ?division)
          (same ?meeting-division whole-company)
        )
      )
    )
  )
)
(repl '(meeting-time (Hacker Alyssa P) (Wednesday . ?rest)))
; {meeting-time {Hacker Alyssa P} {Wednesday 4pm}}
; {meeting-time {Hacker Alyssa P} {Wednesday 3pm}}

; しかし、一部のクエリではオリジナルの環境と異なるふるまいをする。
; まず、notが正常に動作しなくなるというものがある
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
(repl '(lives-near (Hacker Alyssa P) ?x))
; 空のストリーム

; これは、andのすべての節が同時に処理されてしまうためにnotがフィルタの役割を果たしていないためである。
; 上記のクエリでは、(same ?person-1 ?person-2)は空のフレームで評価されるため常に成り立つ。
; そのためnotが空のフレームストリームを生成し、結果もまた空になってしまった。

; したがって、notをはずすことで結果が表示されるようになる。

(repl 
  '
  (assert! 
    (rule 
      (lives-near2 ?person-1 ?person-2)
      (and 
        (address ?person-1 (?town . ?rest-1))
        (address ?person-2 (?town . ?rest-2))
      )
    )
  )
)
(repl '(lives-near2 (Hacker Alyssa P) ?x))
; {lives-near2 {Hacker Alyssa P} {Fect Cy D}}
; {lives-near2 {Hacker Alyssa P} {Hacker Alyssa P}}

; また、再帰についてもオリジナルのふるまいと異なる場合がある。
(repl 
  '(assert!
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
(repl '(outranked-by (Reasoner Louis) ?who))
; {outranked-by {Reasoner Louis} {Hacker Alyssa P}}
; 以降停止しない

; オリジナルの環境であれば、outranked-byの再帰呼出しは、(supervisor ?staff-person ?middle-manager)が束縛をフレームに追加していくことで停止した。
; しかし本アプローチでは、outranked-byの再帰呼出しは常にandへ渡されるフレームで評価される。
; したがって、(supervisor ?staff-person ?middle-manager)による「フィルタ」が機能しなくなったため、無限ループが発生した。
; この事象は、練習問題4.64で発生した無限ループのものと同じである。
