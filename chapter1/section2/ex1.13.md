※VS Code の Markdown Preview Enhanced で表示してください．

---

問題：（$0$以上の自然数$n$について）$Fib(n)$が$\varphi^n / \sqrt{5}$に最も近い整数であることを証明せよ．$\varphi = (1 + \sqrt{5}) / 2$とする

証明：

まず，数学的帰納法により

$$
\begin{align*}
 Fib(n) &= \frac{\varphi^n - \psi^n}{\sqrt{5}} \qquad (1) \\
\varphi &= \frac{1 + \sqrt{5}}{2} \\
   \psi &= \frac{1 - \sqrt{5}}{2}
\end{align*}
$$

を示す．

$n = 0$の場合，

$$
\begin{align*}
Fib(0) &= \frac{\varphi^0 - \psi^0}{\sqrt{5}} \\
       &= \frac{1 - 1}{\sqrt{5}} \\
       &= 0
\end{align*}
$$

より，$(1)$が成り立つ．

$n = 1$の場合，

$$
\begin{align*}
Fib(1) &= \frac{\varphi^1 - \psi^1}{\sqrt{5}} \\
       &= \frac{1 + \sqrt{5} - 1 + \sqrt{5}}{2} \frac{1}{\sqrt{5}} \\
       &= 1
\end{align*}
$$

より，$(1)$が成り立つ．

$n > 1$の場合，$n - 1$と$n - 2$で$(1)$が成り立つと仮定し，$n$でも$(1)$が成り立つことを示す．

$$
\begin{align*}
Fib(n) &= Fib(n - 1) + Fib(n - 2) \\
       &= \frac{\varphi^{n-1} - \psi^{n-1}}{\sqrt{5}} + \frac{\varphi^{n-2} - \psi^{n-2}}{\sqrt{5}} \\
       &= \frac{1}{\sqrt{5}} (\varphi^{n-1} - \psi^{n-1} + \varphi^{n-2} - \psi^{n-2}) \\
       &= \frac{1}{\sqrt{5}} (\varphi \cdot \varphi^{n-2} +  \varphi^{n-2} - \psi \cdot \psi^{n-2} - \psi^{n-2}) \\
       &= \frac{1}{\sqrt{5}}\{\varphi^{n-2}(\varphi + 1) - \psi^{n-2}(\psi + 1)\}
\end{align*}
$$

である．

ここで，

$$
\begin{align*}
\varphi + 1 &= \frac{1 + \sqrt{5}}{2} + 1 \\
            &= \frac{1 + \sqrt{5} + 2}{2} \\
            &= \frac{2 + 2\sqrt{5} + 4}{4} \\
            &= \frac{1 + 2\sqrt{5} + 5}{4} \\
            &= \frac{1 + 2\sqrt{5} + 5}{4} \\
            &= \frac{(1 + \sqrt{5})^2}{2^2} \\
            &= \varphi^2
\end{align*}
$$

である．

また，

$$
\begin{align*}
\psi + 1 &= \frac{1 - \sqrt{5}}{2} + 1 \\
         &= \frac{1 - \sqrt{5} + 2}{2} \\
         &= \frac{2 - 2\sqrt{5} + 4}{4} \\
         &= \frac{1 - 2\sqrt{5} + 5}{4} \\
         &= \frac{(1 - \sqrt{5})^2}{2^2} \\
         &= \psi^2 \\
\end{align*}
$$

である．

したがって，

$$
\begin{align*}
Fib(n) &= \frac{1}{\sqrt{5}}\{\varphi^{n-2}(\varphi + 1) - \psi^{n-2}(\psi + 1)\} \\
       &= \frac{1}{\sqrt{5}}(\varphi^{n-2} \cdot \varphi^2 - \psi^{n-2} \cdot \psi^2) \\
       &= \frac{\varphi^n - \psi^n}{\sqrt{5}}
\end{align*}
$$

となり，$(1)$が成り立つ．

以上の議論から，$0$以上の自然数$n$について$(1)$が成り立つ．

---

次に，$0$以上の自然数$n$について

$$
\left| \frac{\psi^n}{\sqrt{5}} \right| < \frac{1}{2} \qquad (2)
$$

を数学的帰納法によって示す．

$n = 0$の場合，

$$
\begin{align*}
\left| \frac{\psi^0}{\sqrt{5}} \right| &< \frac{1}{2} \\
\left| \frac{1}{\sqrt{5}} \right| &< \frac{1}{2} \\
\frac{1}{\sqrt{5}} &< \frac{1}{\sqrt{4}} \\
\end{align*}
$$

より$(2)$は成り立つ．

$n > 0$の場合，$n - 1$で$(2)$が成り立つと仮定して，$n$でも$(2)$が成り立つことを示す．

ここで，

$$
\begin{align*}
\left| \psi \right| &< 1 \\
\left| \frac{1 - \sqrt{5}}{2} \right| &< 1 \\
\frac{\sqrt{5} - 1}{2} &< 1 \\
\sqrt{5} - 1 &< 2 \\
\sqrt{5} &< 3 \\
\sqrt{5} &< \sqrt{9} \\
\end{align*}
$$

より，$|\psi| < 1$であることと帰納法の仮定を利用すると，

$$
\begin{align*}
\left| \frac{\psi^{n}}{\sqrt{5}} \right|
= \left| \frac{\psi^{n - 1} \cdot \psi}{\sqrt{5}} \right|
< \left| \frac{\psi^{n - 1}}{\sqrt{5}} \right| &< \frac{1}{2} \\
\end{align*}
$$

より$(2)$は成り立つ．

したがって，$0$以上の自然数$n$について$(2)$は成り立つ．

---

以上の議論から当初の問題を示す．$(1)$より

$$
Fib(n) = \frac{\varphi^n - \psi^n}{\sqrt{5}} = \frac{\varphi^n}{\sqrt{5}} - \frac{\psi^n}{\sqrt{5}}
$$

であり，この値は整数である．$(2)$より，

$$
\left| \frac{\psi^n}{\sqrt{5}} \right| < \frac{1}{2}
$$

であるから，

$$
\frac{\varphi^n}{\sqrt{5}}
$$

で$Fib(n)$を近似したとしても，その値は真の整数値から$1/2$以上離れない．したがって，$Fib(n)$は$\varphi^n / \sqrt{5}$に最も近い整数である．
