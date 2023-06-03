定義が逐次的に解釈される場合において、e3 を評価するときの環境の図を示す。

```scheme
(
  (lambda ⟨vars⟩
    (define u ⟨e1⟩)
    (define v ⟨e2⟩)
    ⟨e3⟩
  )
)
```

```text
global env ----------------+
| primitive procedures...  |
+--------------------------+

E1 ((lambda ⟨vars⟩ ... )) --+
| ⟨vars⟩の束縛              |--> global env
| u: ⟨e1⟩                   |
| v: ⟨e2⟩                   |
+---------------------------+
```

e3 は E1 において評価される。

次に、定義を掃き出す場合において、e3 を評価するときの環境の図を示す。

```scheme
(
  (lambda ⟨vars⟩
    (let
      ( ;
        (u '*unassigned* )
        (v '*unassigned* )
      )

      (set! u ⟨e1⟩)
      (set! v ⟨e2⟩)
      ⟨e3⟩
    )
  )
)
```

このプログラムは以下のように展開される

```scheme
(
  (lambda ⟨vars⟩
    (
      (lambda (u v)
        (set! u ⟨e1⟩)
        (set! v ⟨e2⟩)
        ⟨e3⟩
      )
      '*unassigned*
      '*unassigned*
    )
  )
)
```

```text
global env ----------------+
| primitive procedures...  |
+--------------------------+

E1 ((lambda ⟨vars⟩ ... )) --+
| ⟨vars⟩の束縛              |--> global env
+---------------------------+

E2 ((lambda (u v) ...)) --+
| u: ⟨e1⟩                 |--> E1
| v: ⟨e2⟩                 |
+-------------------------+
```

e3 は E2 において評価される。

> 変形したプログラムでは、なぜフレームがひとつ余分にあるのだろうか。

変形に let を用いており、それが lambda に展開され、フレームが作成されるからである。

> 環境構造のこの違いが正しいプログラムのふるまいには決して影響を及ぼさない理由を説明せよ。

すべての式について、それが評価されるときに参照できる変数が展開前後で変化しないことを確認すれば十分である。

e1 について、展開前後で vars のみに参照できることに変わりがない。その他の変数に関しては、展開前では Unbound エラーになり、展開後では unassigned を参照することによるエラーになる。同様に、e2 については vars と u を参照できるがそれ以外ではエラーになる。e3 については、すべての変数を参照できる。

> インタプリタに余分なフレームを構築することなしに “同時” スコープルールを実装させる方法を設計せよ。

let の使用をやめて、例えば以下のように展開することで可能になる。

```scheme
(
  (lambda ⟨vars⟩
    ; body内のdefineを走査し、lambdaの先頭で'*unassigned*として定義するようにする
    (define u '*unassigned*)
    (define v '*unassigned*)

    ; もともとのdefineはset!で置き換える
    (set! u ⟨e1⟩)
    (set! v ⟨e2⟩)
    ⟨e3⟩
  )
)
```
