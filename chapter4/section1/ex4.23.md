Alyssa の analyze-sequence は、列のループを構文解析時ではなくランタイムで行う。つまり、列をループするための car や cdr、null?がランタイムで実行される。一方で、本文中の analyze-sequence はそのような処理を行わない。

例えば、列の長さが 1 の場合を考える。Alyssa の analyze-sequence は、ランタイムに（env が渡されたときに）execute-sequence を実行する。execute-sequence は、列が null ではないことを確かめたうえで列から最初の要素を car し、それを env に適用する。次に自身を再帰的に呼び出し、そこで停止する。一方で本文の analyze-sequence では、列は単に最初の要素（を解析したもの）に評価される。そのため先程のような列の走査は行われない。

列の長さが 2 の場合でも同様である。Alyssa の analyze-sequence は列を走査するためのコストが、列の長さに比例して必要であるのに対して、本文中の手法では追加のコストはない。
