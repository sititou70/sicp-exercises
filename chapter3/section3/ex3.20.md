以下のコード実行後の環境の図を示す。

```scheme
(define
  (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define
    (dispatch m)
    (cond
      ((eq? m 'car) x)
      ((eq? m 'cdr) y)
      ((eq? m 'set-car!) set-x!)
      ((eq? m 'set-cdr!) set-y!)
      (else
       (error " Undefined operation : CONS" m)
      )
    )
  )
  dispatch
)
(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define
  (set-car! z new-value)
  ((z 'set-car!) new-value)
  z
)
(define
  (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z
)

(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)
(car x)
```

```text
global env --+
| cons:      |
| car:       |
| cdr:       |
| set-car!:  |
| set-cdr!:  |
| x:         |
| z:         |
+------------+

cons (global env)
  |
  v
(.)(.)--> global env
 |
 v
parameters: x y
body: (define (set-x! v) ... ) ...

car (global env)
  |
  v
(.)(.)--> global env
 |
 v
parameters: z
body: (z 'car)

cdr (global env)
  |
  v
(.)(.)--> global env
 |
 v
parameters: z
body: (z 'cdr)

set-car! (global env)
  |
  v
(.)(.)--> global env
 |
 v
parameters: z new-value
body: ((z 'set-car!) new-value) z

set-cdr! (global env)
  |
  v
(.)(.)--> global env
 |
 v
parameters: z
body: ((z 'set-cdr!) new-value) z

E1 (cons 1 2) --+
| x: 17         |--> global env
| y: 2          |
| set-x!:       |
| set-y!:       |
| dispatch:     |
+---------------+

set-x! (E1)
  |
  v
(.)(.)--> E1
 |
 v
parameters: v
body: (set! x v)

set-y! (E1)
  |
  v
(.)(.)--> E1
 |
 v
parameters: v
body: (set! y v)

dispatch (E1)
  |
  v
(.)(.)--> E1
 |
 v
parameters: m
body: (cond ...)

x (global env)
  |
  v
dispatch (E1)

E2 (cons x x) ------+
| x: x (global env) |--> global env
| y: x (global env) |
| set-x!:           |
| set-y!:           |
| dispatch:         |
+-------------------+

set-x! (E2)
  |
  v
(.)(.)--> E2
 |
 v
parameters: v
body: (set! x v)

set-y! (E2)
  |
  v
(.)(.)--> E2
 |
 v
parameters: v
body: (set! y v)

dispatch (E2)
  |
  v
(.)(.)--> E2
 |
 v
parameters: m
body: (cond ...)

z (global env)
  |
  v
dispatch (E2)

E3 (cdr z) ---------+
| z: z (global env) |--> global env
+-------------------+

E4 (z 'cdr) --+
| m: 'cdr     |--> E2
+-------------+

E5 (set-car! x 17) --+
| z: x (global env)  |--> global env
| new-value: 17      |
+--------------------+

E6 (x 'set-car!) --+
| m: 'set-car!     |--> E1
+------------------+

E7 (set-x! 17) --+
| v: 17          |--> E1
+----------------+

E8 (car x) ---------+
| z: x (global env) |--> global env
+-------------------+

E9 (x 'car) --+
| m: 'car     |--> E1
+-------------+
```

この動作は ex3.11 のものと似ている。どちらも、ペアの値をコンストラクタ実行時のフレームに束縛された変数として保持している。また、それに対する操作を`dispatch`手続きがメッセージパッシングスタイルで処理している。
