#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "m-eval/eval-apply.scm")
(require "m-eval/global-environment.scm")

; main
; これは、タグを和訳した「日本語Scheme」である。ついでに手続き名や演算子名も和訳している
(eval 
  '(定義
    (インクリメント 引数)
    (和 引数 1)
   )
  the-global-environment
)
; '完了
(eval 
  '(定義
    (二回適用 手続き)
    (ラムダ (引数) (手続き (手続き 引数)))
   )
  the-global-environment
)
; '完了
(eval '(((二回適用 (二回適用 二回適用)) インクリメント) 5) the-global-environment)
; 21

(eval 
  '(定義
    (フィボナッチ 引数)
    (条件分岐 
      ((等しい 引数 0) 0)
      ((等しい 引数 1) 1)
      (それ以外 
        (和 
          (フィボナッチ (差 引数 1))
          (フィボナッチ (差 引数 2))
        )
      )
    )
   )
  the-global-environment
)
; '完了
(eval '(フィボナッチ 10) the-global-environment)
; 55

(eval 
  '(定義
    (引き出し処理器を作成 残高)
    (ラムダ 
      (金額)
      (もし 
        (以上 残高 金額)
        (開始 
          (代入！ 残高 (差 残高 金額))
          残高
        )
        "引き出せるだけの残高がありません"
      )
    )
   )
  the-global-environment
)
; '完了
(eval '(定義 引き出し処理器 (引き出し処理器を作成 100)) the-global-environment)
; '完了
(eval '(引き出し処理器 20) the-global-environment)
; 80
(eval '(引き出し処理器 30) the-global-environment)
; 50
(eval '(引き出し処理器 100) the-global-environment)
; "引き出せるだけの残高がありません"
