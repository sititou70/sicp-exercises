再帰 factorial 手続きで n!を計算するときのスタックに関する統計：

|                               | push の総数 | 最大深度 |
| :---------------------------- | :---------- | :------- |
| 特別目的マシン                | 2n - 1      | 2n - 1   |
| コンパイル版                  | 6n - 4      | 3n - 1   |
| 解釈版                        | 32n - 16    | 5n + 3   |
| 特別目的マシン / コンパイル版 | 1 / 3       | 2 / 3    |
| 解釈版 / コンパイル版         | 16 / 3      | 5 / 3    |

b. 例えばオープンコードによる最適化が可能である。

図 5.17 のコードを見ると、`(= n 1)`を計算する直前に env や continue を保存している。しかし、`=`は基本手続きであるため env も continue も保存する必要がない。これはオープンコードの最適化で削除されるスタック演算である。

また、`*`の計算を行う際に proc と argl を保存しているが、これらもオープンコードの最適化によって必要なくなる。オープンコードでは演算子は適用時に op 命令の引数として直接参照されるため proc を保存する必要はない。また factorial では 2 番目に評価される引数が単なる変数参照であるため、arg1 を保存することも必要ない。

オープンコード最適化をサポートするコンパイラによる factorial のコンパイル結果を以下に示す。

```text
{assign val {op make-compiled-procedure} {label entry1} {reg env}}
{goto {label after-lambda2}}

entry1
{assign env {op compiled-procedure-env} {reg proc}}
{assign env {op extend-environment} {const {n}} {reg argl} {reg env}}
{assign arg1 {op lookup-variable-value} {const n} {reg env}}
{assign arg2 {const 1}}
{assign val {op =} {reg arg1} {reg arg2}}
{test {op false?} {reg val}}
{branch {label false-branch4}}

true-branch3
{assign val {const 1}}
{goto {reg continue}}

false-branch4
{save continue}
{save env}
{assign proc {op lookup-variable-value} {const factorial} {reg env}}
{assign arg1 {op lookup-variable-value} {const n} {reg env}}
{assign arg2 {const 1}}
{assign val {op -} {reg arg1} {reg arg2}}
{assign argl {op list} {reg val}}
{test {op primitive-procedure?} {reg proc}}
{branch {label primitive-branch6}}

compiled-branch7
{assign continue {label proc-return9}}
{assign val {op compiled-procedure-entry} {reg proc}}
{goto {reg val}}

proc-return9
{assign arg1 {reg val}}
{goto {label after-call8}}

primitive-branch6
{assign arg1 {op apply-primitive-procedure} {reg proc} {reg argl}}

after-call8
{restore env}
{assign arg2 {op lookup-variable-value} {const n} {reg env}}
{assign val {op *} {reg arg1} {reg arg2}}
{restore continue}
{goto {reg continue}}

after-if5
after-lambda2
{perform {op define-variable!} {const factorial} {reg val} {reg env}}
{assign val {const ok}}
```

結果として、save 命令の個数は 6 個 -> 2 個に減少した。これは手書きによるコントローラのコードと同じ個数である。
