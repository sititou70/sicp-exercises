#lang racket
(provide (all-defined-out))

(require sicp)
(require "compiler/compile.rkt")
(require "compiler/instruction-sequence.rkt")

; main
;; 再帰版
(map 
  displayln
  (statements 
    (compile 
      '
      (define 
        (factorial n)
        (define 
          (iter product counter)
          (if (> counter n) 
            product
            (iter 
              (* counter product)
              (+ counter 1)
            )
          )
        )
        (iter 1 1)
      )
      'val
      'next
    )
  )
)

;; 反復版
(displayln "rec")
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

; 反復版のコンパイル結果に注釈をつけたもの
; {assign val {op make-compiled-procedure} {label entry1} {reg env}} factorialの定義
; {goto {label after-lambda2}} 変数の定義と結果（ok）の代入にジャンプ
; 
; entry1 factrialのエントリーポイント
; {assign env {op compiled-procedure-env} {reg proc}} 手続きの環境を取り出す
; {assign env {op extend-environment} {const {n}} {reg argl} {reg env}} extendsする
; {assign val {op make-compiled-procedure} {label entry3} {reg env}} iterを作成
; {goto {label after-lambda4}} iterの定義と結果（ok）の代入にジャンプ
; 
; entry3 iterのエントリーポイント
; {assign env {op compiled-procedure-env} {reg proc}} 手続きの環境を取り出す
; {assign env {op extend-environment} {const {product counter}} {reg argl} {reg env}} 環境をextend
; {save continue} 条件分岐の条件節を求める
; {save env}
; {assign proc {op lookup-variable-value} {const >} {reg env}} 大小比較の演算子評価
; {assign val {op lookup-variable-value} {const n} {reg env}} 大小比較の被演算子リスト作成
; {assign argl {op list} {reg val}}
; {assign val {op lookup-variable-value} {const counter} {reg env}}
; {assign argl {op cons} {reg val} {reg argl}}
; {test {op primitive-procedure?} {reg proc}} 大小比較、基本手続きと複合手続きの分岐
; {branch {label primitive-branch8}}
; 
; compiled-branch9 「>」演算子が複合手続きの場合のブランチ。ここには入らない。
; {assign continue {label after-call10}}
; {assign val {op compiled-procedure-entry} {reg proc}}
; {goto {reg val}}
; 
; primitive-branch8 「>」演算子が基本手続きの場合のブランチ。ここに入る。
; {assign val {op apply-primitive-procedure} {reg proc} {reg argl}}
; 
; after-call10 大小比較完了
; {restore env}
; {restore continue}
; {test {op false?} {reg val}} 条件分岐を行う
; {branch {label false-branch6}}
; 
; true-branch5 条件分岐が真、productをvalに入れて継続に帰る
; {assign val {op lookup-variable-value} {const product} {reg env}}
; {goto {reg continue}}
; 
; false-branch6 条件分岐が偽、まだ残りの計算がある場合
; {assign proc {op lookup-variable-value} {const iter} {reg env}} 再帰呼出し、演算子の評価
; {save continue} 再帰呼出し、被演算子リストの構築
; {save proc}
; {save env}
; {assign proc {op lookup-variable-value} {const +} {reg env}} 加算、演算子の評価
; {assign val {const 1}} 加算、被演算子リストの構築
; {assign argl {op list} {reg val}}
; {assign val {op lookup-variable-value} {const counter} {reg env}}
; {assign argl {op cons} {reg val} {reg argl}}
; {test {op primitive-procedure?} {reg proc}} 加算、基本手続きと複合手続きの分岐
; {branch {label primitive-branch14}}
; 
; compiled-branch15 加算が複合手続きのブランチ、ここには入らない
; {assign continue {label after-call16}}
; {assign val {op compiled-procedure-entry} {reg proc}}
; {goto {reg val}}
; 
; primitive-branch14 加算が基本手続きのブランチ、ここに入る
; {assign val {op apply-primitive-procedure} {reg proc} {reg argl}}
; 
; after-call16 加算完了
; {assign argl {op list} {reg val}} arglの最初の要素として代入
; {restore env} 加算によって変更されたenvを復元（今回は基本手続きなので実際には変更されない）
; {save argl}
; {assign proc {op lookup-variable-value} {const *} {reg env}} 乗算、演算子の評価
; {assign val {op lookup-variable-value} {const product} {reg env}} 乗算、被演算子リストの構築
; {assign argl {op list} {reg val}}
; {assign val {op lookup-variable-value} {const counter} {reg env}}
; {assign argl {op cons} {reg val} {reg argl}}
; {test {op primitive-procedure?} {reg proc}} 乗算、基本手続きと複合手続きの分岐
; {branch {label primitive-branch11}}
; 
; compiled-branch12 乗算が複合手続きのブランチ、ここには入らない
; {assign continue {label after-call13}}
; {assign val {op compiled-procedure-entry} {reg proc}}
; {goto {reg val}}
; 
; primitive-branch11 乗算が基本手続き、ここに入る
; {assign val {op apply-primitive-procedure} {reg proc} {reg argl}}
; 
; after-call13 乗算完了
; {restore argl} 乗算によって変更されたarglを復元
; {assign argl {op cons} {reg val} {reg argl}} arglに乗算結果を追加
; {restore proc}
; {restore continue}
; {test {op primitive-procedure?} {reg proc}} 再帰呼出し、基本手続きと複合手続きの分岐
; {branch {label primitive-branch17}}
; 
; compiled-branch18 再帰呼出しが複合手続きの場合のブランチ、ここに入る
; {assign val {op compiled-procedure-entry} {reg proc}} iterのエントリーポイントを取得
; {goto {reg val}} エントリーポイントにジャンプ
; 
; primitive-branch17 再帰呼出しが基本手続きのブランチ、ここには入らない
; {assign val {op apply-primitive-procedure} {reg proc} {reg argl}}
; {goto {reg continue}}
; 
; after-call19
; after-if7
; after-lambda4 iterの定義と結果の代入
; {perform {op define-variable!} {const iter} {reg val} {reg env}} iterを宣言
; {assign val {const ok}} 結果を代入
; {assign proc {op lookup-variable-value} {const iter} {reg env}} iterの初期呼び出し、演算子の評価
; {assign val {const 1}} iterの初期呼び出し、被演算子リストの構築
; {assign argl {op list} {reg val}}
; {assign val {const 1}}
; {assign argl {op cons} {reg val} {reg argl}}
; {test {op primitive-procedure?} {reg proc}} iterの初期呼び出し、基本手続きと複合手続きの分岐
; {branch {label primitive-branch20}}
; 
; compiled-branch21 iterが複合手続きのブランチ、ここに入る
; {assign val {op compiled-procedure-entry} {reg proc}} エントリーポイントを取得
; {goto {reg val}} エントリーポイントにジャンプ
; 
; primitive-branch20 iterが基本手続きのブランチ、ここには入らない
; {assign val {op apply-primitive-procedure} {reg proc} {reg argl}}
; {goto {reg continue}}
; 
; 変数定義と結果の代入
; after-call22
; after-lambda2
; {perform {op define-variable!} {const factorial} {reg val} {reg env}}
; {assign val {const ok}}

; 反復版と再帰版で、片方がスタック空間を積み上げていき、もう片方が一定のスタック空間で動作する理由は、factrialの最後の式付近にある。
; 反復版では以下のようなコードになっている。save/restoreのペアと重要な分岐以外は省略してある。

; {assign proc {op lookup-variable-value} {const iter} {reg env}} 再帰呼出し、演算子の評価
; {save continue} 再帰呼出し、被演算子リストの構築
; {save proc}
; {save env}
; 加算、演算子の評価
; 加算、被演算子リストの構築
; {restore env}
; {save argl}
; 乗算、演算子の評価
; 乗算、被演算子リストの構築
; {restore argl}
; {restore proc}
; {restore continue}
; {test {op primitive-procedure?} {reg proc}} 再帰呼出し、基本手続きと複合手続きの分岐
; {branch {label primitive-branch17}}
; 
; compiled-branch18 再帰呼出しが複合手続きの場合のブランチ、ここに入る
; {assign val {op compiled-procedure-entry} {reg proc}} iterのエントリーポイントを取得
; {goto {reg val}} エントリーポイントにジャンプ
; 
; primitive-branch17 再帰呼出しが基本手続きのブランチ、ここには入らない

; save/restoreのペアを確認すると、再帰呼出しのエントリーポイントへジャンプするまでに、スタックの内容が増えていないとわかる
; また、compiled-branch18を見ると再帰呼出しは単なるジャンプによって行われており、呼び出し後の継続処理が無いとわかる
; したがって、反復版factorialは、どれだけ再帰呼出しを行ったとしてもスタック空間が一定で済む

; 一方、再帰版のコードは以下のようになる。

; 乗算、演算子の評価
; {save continue}
; 乗算、被演算子リストの構築
; {save proc}
; {save argl}
; 再帰呼出し、演算子の評価（乗算の被演算子リストの構築中であることに注意）
; 再帰呼出し、被演算子リストの構築
; {save proc}
; 減算、演算子の評価
; 減算、被演算子リストの構築
; {restore proc}
; {test {op primitive-procedure?} {reg proc}} 再帰呼出し、基本手続きと複合手続きの分岐
; {branch {label primitive-branch34}}
; 
; compiled-branch35 再帰呼出しが複合手続き、ここに入る
; {assign continue {label after-call36}} 継続（乗算の被演算子リストの構築）に帰るようにセット
; {assign val {op compiled-procedure-entry} {reg proc}} 再帰呼出しのエントリーポイントを取得
; {goto {reg val}} エントリーポイントにジャンプ
; 
; primitive-branch34 再帰呼出しが基本手続き、ここには入らない
; 
; after-call36 再帰呼出し完了
; {restore argl}
; {assign argl {op cons} {reg val} {reg argl}} 結果をarglに追加
; {restore proc}
; {restore continue}
; {test {op primitive-procedure?} {reg proc}} 乗算、基本手続きと複合手続きの分岐
; {branch {label primitive-branch37}}
; 
; compiled-branch38 乗算が複合手続き、ここには入らない
; 
; primitive-branch37 乗算が基本手続き、ここに入る
; 乗算実行
; 継続に帰る

; 再帰呼出しは、乗算の被演算子リストの構築中に実行される。

; そのため、再帰呼出し中に以下のレジスタをスタックに積む必要がある。
; proc：現在処理している演算子が乗算であることを覚えておく必要がある
; continue：再帰呼出しが完了したら、乗算の被演算子リストの構築を継続するということを覚えておく必要がある

; したがって、再帰呼出しの回数に応じてスタック空間を消費してしまう
