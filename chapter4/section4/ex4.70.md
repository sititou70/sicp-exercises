`add-assertion!`の定義は以下のようなものである。

```scheme
(define
  (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let
    ( ;
     (old-assertions THE-ASSERTIONS)
    )

    (set!
      THE-ASSERTIONS
      (cons-stream assertion old-assertions)
    )
    'ok
  )
)
```

ここで、old-assertions の束縛がないと、assertion を THE-ASSERTIONS に追加したことにはならない。

なぜなら、`cons-stream`の第 2 引数は遅延されるため、参照時点の環境における THE-ASSERTIONS を探索してしまうからである。このコードでは、グローバル環境の THE-ASSERTIONS を参照してしまう。

それを防ぐために、old-assertions によって環境にその時点での THE-ASSERTIONS の値を束縛し、保存している。こうすることで、後から old-assertions が参照されたとしても、それは古い THE-ASSERTIONS の値を保持しており、想定通りの動作となる。

add-rule!についても同様である。
