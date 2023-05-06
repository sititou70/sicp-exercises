let を使わない make-withdraw は以下のとおりである。

```scheme
(define
  (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance
      )
      " Insufficient funds "
    )
  )
)
(define W1 (make-withdraw 100))
(W1 50)
(define W2 (make-withdraw 100))
```

これに対応する環境構造は以下のとおりである。

```text
global env --------------------+
| make-withdraw:   W1:   W2:   |
+------------------------------+

make-withdraw
  |
  v
(.)(.)--> global env
 |
 v
parameters: balance
body: (lambda (amount) ... )

E1 (make-withdraw 100) --+
| balance: 50            |--> global env
+------------------------+

  W1
  |
  v
(.)(.)--> E1
 |
 v
parameters: amount
body: (if (>= balance amount) ... )

E2 (W1 50) --+
| amount: 50 |--> E1
+------------+

E3 (make-withdraw 100) --+
| balance: 100           |--> global env
+------------------------+

  W2
  |
  v
(.)(.)--> E3
 |
 v
W1のコードと同じ
```

let を使用した make-withdraw は以下の通りである。

```scheme
(define
  (make-withdraw initial-amount)
  (let
    ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance
        )
        " Insufficient funds "
      )
    )
  )
)
(define W1 (make-withdraw 100))
(W1 50)
(define W2 (make-withdraw 100))
```

これに対応する環境構造は以下のとおりである。

```text
global env --------------------+
| make-withdraw:   W1:   W2:   |
+------------------------------+

make-withdraw
  |
  v
(.)(.)--> global env
 |
 v
parameters: initial-amount
body: (let ... )

E1 (make-withdraw 100) --+
| initial-amount: 100    |--> global env
+------------------------+

E2 (let ... ) --+
| balance: 50   |--> E1
+---------------+

  W1
  |
  v
(.)(.)--> E2
 |
 v
parameters: amount
body: (if (>= balance amount) ... )

E3 (W1 50) --+
| amount: 50 |--> E2
+------------+

E4 (make-withdraw 100) --+
| initial-amount: 100    |--> global env
+------------------------+

E5 (let ... ) --+
| balance: 100  |--> E4
+---------------+

  W2
  |
  v
(.)(.)--> E5
 |
 v
W1のコードと同じ
```

let を使用した方の環境構造では、E1 や E4 といった余計なフレームが増えている。これらのフレームに束縛されている変数はその後参照、更新されない。
