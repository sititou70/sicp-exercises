以下のような solve 手続きについて考える。

```scheme
(define (solve f y0 dt)
  (define y ( integral (delay dy) y0 dt))
  (define dy ( stream-map f y))
  y
)
```

これを、問題文中の戦略によって掃き出すと以下のようになる。

```scheme
(define
  solve
  (lambda (f y0 dt)
    (let
      ( ;
       (y '*unassigned*)
       (dy '*unassigned*)
      )

      (let
        ( ;
         (temp-y (integral (delay dy) y0 dt))
         (temp-dy (stream-map f y))
        )

        (set! y temp-y)
        (set! dy temp-dy)
      )

      y
    )
  )
)
```

これは以下のように展開される。

```scheme
(define
  solve
  (lambda (f y0 dt)
    ((lambda (y dy)
       ((lambda (temp-y temp-dy)
          (set! y temp-y)
          (set! dy temp-dy)
        )
         (integral (delay dy) y0 dt)
         (stream-map f y) ; ... (a)
       )

       y
     )
      '*unassigned*
      '*unassigned*
    )
  )
)
```

これは動作しない。(a)の式は application であるから、list-of-values によって被演算子が評価される。そのとき y 参照されるが、 unassigned エラーになる。

一方で、本文で採用されている掃き出し戦略を使用すると以下のようになる。

```scheme
(define
  solve
  (lambda (f y0 dt)
    (let
      ( ;
       (y '*unassigned*)
       (dy '*unassigned*)
      )

      (set! y (integral (delay dy) y0 dt))
      (set! dy (stream-map f y))
      y
    )
  )
)
```

これは以下のように展開される。

```scheme
(define
  solve
  (lambda (f y0 dt)
    ((lambda (y dy)
       (set! y (integral (delay dy) y0 dt))
       (set! dy (stream-map f y)) ; ... (b)
       y
     )
      '*unassigned*
      '*unassigned*
    )
  )
)
```

これは動作する。(b)で y を参照する場合、先程の(a)とは違って既に y がセットされているからである。

問題文で示された戦略は「定義された変数の値はそれらの変数の値を使わないでも評価できるという制約を強制」するものである。
