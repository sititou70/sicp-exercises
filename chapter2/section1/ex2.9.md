2 つの区間を$[a_1, a_2]$と$$[b_1, b_2]$とおく。

それぞれの幅は

$$
w_a = \frac{a_2 - a_1}{2} \\
w_b = \frac{b_2 - b_1}{2}
$$

となる。

2 つの区間の和は$[a_1 + b_1, a_2 + b_2]$であり、その差は

$$
\frac{(a_2 + b_2) - (a_1 + b_1)}{2} \\
= \frac{a_2 + b_2 - a_1 - b_1}{2} \\
= \frac{a_2 + - a_1 + b_2 - b_1}{2} \\
= \frac{(a_2 + - a_1) + (b_2 - b_1)}{2} \\
= w_a + w_b
$$

となり、もとの 2 つの区間の差の関数として表せる。

一方、乗算に関してはもとの 2 つの区間の差の関数としては表せない。

例えば、同じ差を持つ複数の乗算の結果が異なる場合がある。

```scheme
(mul-interval (make-interval 1 2) (make-interval 1 2))
; '(1 . 4)

(mul-interval (make-interval 10 11) (make-interval 10 11))
; '(100 . 121)
```