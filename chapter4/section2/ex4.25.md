以下の定義は、言語が正規順序なのか適用順序なのかによって動作が異なる。

```scheme
(define
  (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value)
)

(define
  (factorial n)
  (unless
    (= n 1)
    (* n (factorial (- n 1)))
    1
  )
)

(factorial 5)
```

適用順序の言語の場合、これは停止しない。unless の被演算子を先に評価しようとするため、factorial の再帰呼出しが停止しないからである。

正規順のの場合、これは適切に 5 の階乗を計算する。unless の被演算子にある再帰呼出しは、condition が真のときに評価されず停止する。
