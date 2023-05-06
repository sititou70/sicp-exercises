以下のコードを実行したあとの環境構造を示す。

```scheme
(define
  (make-account balance)
  (define
    (withdraw amount)
    (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance
      )
      " Insufficient funds "
    )
  )
  (define
    (deposit amount)
    (set! balance (+ balance amount))
    balance
  )
  (define
    (dispatch m)
    (cond
      ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
      (else
       (error
         " Unknown request : MAKE-ACCOUNT "
         m
       )
      )
    )
  )
  dispatch
)
(define acc (make-account 50))
((acc 'deposit) 40)
((acc 'withdraw) 60)
(define acc2 (make-account 100))
```

環境構造は以下のようになる。

```text
global env ----------------------+
| make-account:   acc:   acc2:   |
+--------------------------------+

make-account
  |
  v
(.)(.)--> global env
 |
 v
parameters: balance
body: (define ... )

E1 (make-account 50) --+
| balance: 30          |--> global env
| withdraw:            |
| deposit:             |
| dispatch:            |
+----------------------+

withdraw (E1)
  |
  v
(.)(.)--> E1
 |
 v
parameters: amount
body: (if (>= balance amount) ... )

deposit (E1)
  |
  v
(.)(.)--> E1
 |
 v
parameters: amount
body: (set! balance ... )

dispatch (E1)
  |
  v
(.)(.)--> E1
 |
 v
parameters: m
body: (cond ... )

 acc
  |
  v
dispatch (E1)と同じ

E2 (acc 'deposit) --+
| m: 'deposit       |--> E1
+-------------------+

E3 (deposit 40) --+
| amount: 40      |--> E1
+-----------------+

E4 (acc 'withdraw) --+
| m: 'withdraw       |--> E1
+--------------------+

E5 (withdraw 60) --+
| amount: 60       |--> E1
+------------ -----+

E6 (make-account 100) --+
| balance: 100          |--> global env
| withdraw:             |
| deposit:              |
| dispatch:             |
+-----------------------+

withdraw (E6)
  |
  v
(.)(.)--> E6
 |
 v
parameters: amount
body: (if (>= balance amount) ... )

deposit (E6)
  |
  v
(.)(.)--> E6
 |
 v
parameters: amount
body: (set! balance ... )

dispatch (E6)
  |
  v
(.)(.)--> E6
 |
 v
parameters: m
body: (cond ... )

 acc2
  |
  v
dispatch (E6)と同じ
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
+---------------+-----+--------+

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

acc と acc2 はグローバル環境しか共有していない。
