2 つの区間の中心をそれぞれ$a$、$b$とおく。またその許容誤差を $E_a$、$E_b$とおく。それぞれの区間は$[a - E_a, a + E_a]$、$[b - E_b, b + E_b]$と表される。

パーセント標準誤差はそれぞれ

$$
\begin{align*}
PE_a &= 100 \frac{E_a}{a} \\
PE_b &= 100 \frac{E_b}{b}
\end{align*}
$$

となる。

すべての数値が正だと仮定すると、$a \cdot b$は

$$
[ab - a E_b - b E_a + E_a E_b, ab + a E_b + b E_a + E_a E_b]
$$

となる。ここで$E_a E_b$は非常に小さいため無視すると

$$
[ab - a E_b - b E_a, ab + a E_b + b E_a]
$$

となる。

この区間の標準誤差$E$は

$$
\begin{align*}
2E &= ab + a E_b + b E_a - (ab - a E_b - b E_a) \\
   &= ab + a E_b + b E_a - ab + a E_b + b E_a \\
   &= 2 a E_b + 2 b E_a \\
E  &= a E_b + b E_a
\end{align*}
$$

また中心$C$は

$$
\begin{align*}
C &= 区間の始点 + E \\
  &= ab - a E_b - b E_a + a E_b + b E_a \\
  &= ab
\end{align*}
$$

よってパーセント標準誤差$PE$は

$$
\begin{align*}
PE &= 100 \frac{E}{C} \\
   &= 100 \frac{a E_b + b E_a}{ab} \\
   &= 100 (\frac{E_a}{a} + \frac{E_b}{b}) \\
   &= 100 (PE_a + PE_b)
\end{align*}
$$
