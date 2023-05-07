フィボナッチ数列を求めるメモ化されたプログラムは以下のとおりである。table に関する定義は省略している。

```scheme
(define
  (memoize f)
  (let
    ((table (make-table)))
    (lambda (x)
      (let
        ((previously-computed-result
           (lookup x table)
         )
        )
        (or previously-computed-result
            (let
              ((result (f x)))
              (insert! x result table)
              result
            )
        )
      )
    )
  )
)

(define
  memo-fib
  (memoize
    (lambda (n)
      (cond
        ((= n 0) 0)
        ((= n 1) 1)
        (else
         (+ (memo-fib (- n 1))
            (memo-fib (- n 2))
         )
        )
      )
    )
  )
)

(memo-fib 3)
```

このプログラムを実行を環境の図を用いて分析する。

memo-fib の定義時：

```text
E1 (memoize (lambda (n) ... )) --+
| f: (lambda (n) ... )           |--> global env
+--------------------------------+

E2 (let ((table ... )) ... ) --+
| table:                       |--> E1
+------------------------------+

table (E2)
  |
  v
空のtable
```

(memo-fib 3)の実行時で、(memo-fib 2)、(memo-fib 1)、(memo-fib 0)が呼ばれた後：

```text
E1 (memoize (lambda (n) ... )) --+
| f: (lambda (n) ... )           |--> global env
+--------------------------------+

E2 (let ((table ... )) ... ) --+
| table:                       |--> E1
+------------------------------+

table (E2)
  |
  v
* 2: 1
* 1: 1
* 0: 0
```

どれもテーブルにヒットしないため、それぞれ実際に計算されて、その結果がセットされる。

この後、(memo-fib 2)、(memo-fib 1)、(memo-fib 0)が更に何度も呼ばれるが、それらの結果はすべてテーブルにヒットするため、実際に計算が実行されることはない。

したがって、n 番目のフィボナッチ数を n に比例するステップ数で計算できる。

一方、これはテーブルへの操作を無視した場合である。例えば本文にある実装のように、テーブルへの操作が、その要素数の線形時間だけかかる場合には、その限りではない。

memo-fib を (memoize fib) と定義した場合について、これでは fib 内の再帰的な呼び出しの結果がメモ化されず、最終結果だけがメモ化されてしまうため、効率が悪くなってしまう。
