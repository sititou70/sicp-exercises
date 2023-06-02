以下のような手続き`halts?`が実装されていると仮定する。

```scheme
; (p a)が停止すればtrue、そうでないならfalse
(define (halts? p a) ...)
```

ここで、以下のような評価を考える

```scheme
(define (run-forever) (run-forever))
(define
  (try p)
  (if (halts? p p) (run-forever) 'halted)
)

; この式aは停止するか？
(try try)
```

a の式`(try try)`が停止すると仮定すると、`(halts? try try)`は偽に評価されたことになる。したがって、`halts?`は`(try try)`を停止しないと判定したことになるが、式 a の仮定と矛盾する。

a の式`(try try)`が停止しないと仮定すると、`(halts? try try)`は真に評価されたことになる。したがって、`halts?`は`(try try)`を停止すると判定したことになるが、式 a の仮定と矛盾する。

どちらの場合も矛盾であるため、`halts?`は実装できないと背理的にわかる。
