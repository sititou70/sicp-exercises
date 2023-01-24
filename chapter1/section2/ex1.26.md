オリジナルの expmod は以下である

```scheme
(define
  (expmod base exp m)
  (cond
    ((= exp 0) 1)
    ((even? exp)
     (remainder
       (square (expmod base (/ exp 2) m))
       m
     )
    )
    (else
     (remainder
       (* base (expmod base (- exp 1) m))
       m
     )
    )
  )
)
```

オリジナルのコードでは，

$$
\begin{align*}
base^{exp} \bmod m &= (base^{exp/2} \cdot base^{exp/2}) \bmod m \\
&= (base^{exp/2} \bmod m) \cdot (base^{exp/2} \bmod m) \bmod m \\
&= (base^{exp/2} \bmod m)^2 \bmod m
\end{align*}
$$

の関係式を以下のように実装することで，

```scheme
     (remainder
       (square (expmod base (/ exp 2) m))
       m
     )
```

`exp`を半分にすることで`expmod`の呼び出し回数もまた半分になり，全体のオーダーは$\Theta(\log n)$となった．

一方，Louis の expmod は以下である．

```scheme
(define
  (expmod base exp m)
  (cond
    ((= exp 0) 1)
    ((even? exp)
     (remainder
       (* (expmod base (/ exp 2) m)
          (expmod base (/ exp 2) m)
       ) ; ここがsquereを使用しないようになっている
       m
     )
    )
    (else
     (remainder
       (* base (expmod base (- exp 1) m))
       m
     )
    )
  )
)
```

`exp`を半分にしているが，そのとき expmod を 2 回呼び出している．したがって，全体としては$\Theta(n)$のオーダーとなってしまった．
