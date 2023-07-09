#lang racket
(provide (all-defined-out))

(require sicp)
(require "compiler/compile.rkt")
(require "compiler/instruction-sequence.rkt")

; main
; 2つの手続きについて、違いは再帰呼出しの引数の順番だけであるため、該当部分だけを抜き出して比較する
;; factorial
(map 
  displayln
  (statements 
    (compile 
      '
      (define 
        (factorial n)
        (if (= n 1) 
          1
          (* (factorial (- n 1)) n)
        )
      )
      'val
      'next
    )
  )
)

;; factorial-alt
(displayln "factorial-alt")
(map 
  displayln
  (statements 
    (compile 
      '
      (define 
        (factorial n)
        (if (= n 1) 
          1
          (* n (factorial (- n 1)))
        )
      )
      'val
      'next
    )
  )
)


; factorialのコンパイル結果
; ...
; 演算子の評価
; {assign proc {op lookup-variable-value} {const *} {reg env}}
; {save continue}
; {save proc}
; 被演算子の評価
; {assign val {op lookup-variable-value} {const n} {reg env}}
; {assign argl {op list} {reg val}}
; {save argl}
; {assign proc {op lookup-variable-value} {const factorial} {reg env}}
; {save proc}
; {assign proc {op lookup-variable-value} {const -} {reg env}}
; {assign val {const 1}}
; {assign argl {op list} {reg val}}
; {assign val {op lookup-variable-value} {const n} {reg env}}
; {assign argl {op cons} {reg val} {reg argl}}
; 手続き適用
; {test {op primitive-procedure?} {reg proc}}
; ...

; factorial-altのコンパイル結果
; ...
; 演算子の評価
; {assign proc {op lookup-variable-value} {const *} {reg env}}
; {save continue}
; {save proc}
; 被演算子の評価
; {save env}
; {assign proc {op lookup-variable-value} {const factorial} {reg env}}
; {save proc}
; {assign proc {op lookup-variable-value} {const -} {reg env}}
; {assign val {const 1}}
; {assign argl {op list} {reg val}}
; {assign val {op lookup-variable-value} {const n} {reg env}}
; {assign argl {op cons} {reg val} {reg argl}}
; 手続き適用
; {test {op primitive-procedure?} {reg proc}}
; ...

; 見つけた違い：
; 前者はarglをsaveしているが後者はしていない。
; これは、前者で2番目に評価される(factorial-alt (- n 1))が手続き適用であり、arglを変更するからである。
; 一方、後者では2番目に評価されるのはnの変数参照であり、これはarglを変更しないためsaveされない。

; 前者ではenvをsaveしていないが後者ではしている。
; 前者では(factorial-alt (- n 1))がenvを変更するが、これは一番最後に評価される引数であるため、envの保存は必要ない（*の環境で適用を実行すればいい）
; 後者では(factorial-alt (- n 1))は最後に評価される引数ではなく、また次に評価するnの変数参照でenvが必要であるためenvがsaveされる。

; 実行効率：
; 命令数を比較すると、前者は14命令であるのに対して、後者は12命令である。
; したがって後者の方が実行しなければならない命令数が少ないため有利である。

; スタックのpush回数については両者とも4回であるため、大きな差は無いと考える。
; （厳密に考えれば、手続きと引数リストの内部表現によっては、一方のpush/popが他方よりも高価になるかもしれない）

; まとめると、後者の方が命令の実行数において効率が良い。
