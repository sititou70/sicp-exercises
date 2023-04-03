オリジナルと Louis の flatmap を以下に示す

オリジナル：

```scheme
(flatmap
  (lambda (rest-of-queens)
    (map
      (lambda (new-row)
        (adjoin-position
          new-row
          k
          rest-of-queens
        )
      )
      ; (enumerate-interval 1 board-size)を実行する
      (enumerate-interval 1 board-size)
    )
  )
  ; queen-cols (- k 1)の各盤面に対して
  (queen-cols (- k 1))
)
```

Louis：

```scheme
(flatmap
  (lambda (new-row)
    (map
      (lambda (rest-of-queens)
        (adjoin-position
          new-row
          k
          rest-of-queens
        )
      )
      ; queen-cols (- k 1)を実行する
      (queen-cols (- k 1))
    )
  )
  ; ボードサイズの各行に対して
  (enumerate-interval 1 board-size)
)
```

k が十分大きい場合、`(queen-cols (- k 1))`は`(enumerate-interval 1 board-size)`よりも時間がかかる。

オリジナルでは`(queen-cols (- k 1))`は 1 回しか実行されていない。一方、Louis のプログラムでは、盤面の 1 辺の長さを$n$とすると、`(queen-cols (- k 1))`は n 回実行されている。そのため Louis のプログラムはオリジナルより動作が遅い。

両者の実行時間を、`safe?`内の map の lambda が実行される回数によって見積もる。

オリジナルのプログラムについて、盤面サイズを$n$、`(length (queen-cols k))`を$L_k$、`(queen-cols k)`で`safe?`内の map の lambda が実行される回数を$S_k$とおくと以下のようになる。

$$
S_k = n L_{k - 1} (k - 1) + S_{k-1}
$$

Louis のプログラムについて、盤面サイズを$n$、`(length (queen-cols k))`を$L_k$、`(queen-cols k)`で`safe?`内の map の lambda が実行される回数を$S_k$とおくと以下のようになる。

$$
S_k = n L_{k - 1} (k - 1) + n S_{k-1}
$$

$L_k$を一般的に求めることは難しいが、個別の値に関しては実際に計算することで求まる。

ex2.43 に示すプログラムは、louis の処理がオリジナルの何倍になるかを、実測と上記式による予測で示す。

例えば盤面サイズが 6 のとき、実行結果は以下のようになる。

```scheme
実測値：44.77504049959291T
予測値：40.1635687732342T
```

盤面のサイズが 6 のとき、Louis のプログラムの実行時間は約$732T$と見積もれる
