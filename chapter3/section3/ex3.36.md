#lang racket/base
(require sicp)

グローバル手続きが定義されるとき

```text
global env ---------------------+
| inform-about-value:           |
| ...                           |
+-------------------------------+

inform-about-value (global env)
  |
  v
(.)(.)--> global env
 |
 v
parameters: constraint
body: (constraint 'I-have-a-value)
```

`(define a (make-connector))`が実行されるとき：

```text
E1 (make-connector) --+
| for-each-except:    |--> global env
+---------------------+

for-each-except (E1)
  |
  v
(.)(.)--> E1
 |
 v
parameters: exception procedure list
body: (define (loop items) ...)

E2 (let ((value false) ...) ...) --+
| value: false                     |--> E1
| informat: false                  |
| constraints: '()                 |
| set-my-value:                    |
| forget-my-value:                 |
| connect:                         |
| me:                              |
+----------------------------------+

set-my-value (E2)
  |
  v
(.)(.)--> E2
 |
 v
parameters: newval setter
body: (cond ...)

forget-my-value (E2)
  |
  v
(.)(.)--> E2
 |
 v
parameters: retractor
body: (if ...)

connect (E2)
  |
  v
(.)(.)--> E2
 |
 v
parameters: new-constraint
body: (if ...)

me (E2)
  |
  v
(.)(.)--> E2
 |
 v
parameters: request
body: (cond ...)

global env ---------------------+
| inform-about-value:           |
| a:                            |
| ...                           |
+-------------------------------+

a (global env)
  |
  v
me (E2)
```

`(define b (make-connector))`が実行されるとき：

```text
E3 (make-connector) --+
| for-each-except:    |--> global env
+---------------------+

for-each-except (E3)
  |
  v
(.)(.)--> E3
 |
 v
parameters: exception procedure list
body: (define (loop items) ...)

E4 (let ((value false) ...) ...) --+
| value: false                     |--> E3
| informat: false                  |
| constraints: '()                 |
| set-my-value:                    |
| forget-my-value:                 |
| connect:                         |
| me:                              |
+----------------------------------+

set-my-value (E4)
  |
  v
(.)(.)--> E4
 |
 v
parameters: newval setter
body: (cond ...)

forget-my-value (E4)
  |
  v
(.)(.)--> E4
 |
 v
parameters: retractor
body: (if ...)

connect (E4)
  |
  v
(.)(.)--> E4
 |
 v
parameters: new-constraint
body: (if ...)

me (E4)
  |
  v
(.)(.)--> E4
 |
 v
parameters: request
body: (cond ...)

global env ---------------------+
| inform-about-value:           |
| a:                            |
| b:                            |
| ...                           |
+-------------------------------+

b (global env)
  |
  v
me (E4)
```

`(set-value! a 10 'user)`が実行されるとき

```text
E5 (set-value! a 10 'user) --+
| connector: a (global env)  |--> global env
| new-value: 10              |
| informant: 'user           |
+----------------------------+

E6 (a 'set-value!) ---+
| request: 'set-value |--> E2
+---------------------+

E7 (set-my-value 10 'user) --+
| newval: 10                 |--> E2
| setter: 'user              |
+----------------------------+

E2 ----------------+
| value: 10        |--> E1
| informat: 'user  |
| constraints: '() |
| set-my-value:    |
| forget-my-value: |
| connect:         |
| me:              |
+------------------+

E8 (for-each-except 'user inform-about-value '()) --+
| exception: 'user                                  |--> E1
| procedure: inform-about-value (global env)        |
| list: '()                                         |
+---------------------------------------------------+
```

となる。
