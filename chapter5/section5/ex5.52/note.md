`compile-to-c.rkt`は、racket のソースコードを本文中のコンパイラによってコンパイルし、その結果を C のコードに変換する。このコードは練習問題 5.51 で実装したマシンおよびユーティリティを使用する。

`compile.sh`は、指定されたファイルをコンパイルし、結果を`main.c`に書き込み、さらに`main.c`をコンパイルして実行可能ファイル`main`を生成する。内部で`compile-to-c.rkt`を使用する。

`sample.rkt`は、Scheme の各言語機能を試すためのサンプルコードである。これはもちろん Racket によって実行できる。

```sh
racket sample.rkt
# 1
# quote_value
# 2
# 3
# ...
```

一方で、今回作成したコンパイラによっても実行できる。

```sh
./compile.sh sample.rkt; ./main
# 1.000000
# quote_value
# 2.000000
# 3.000000
# ...
```

`main.c`の行数をカウントする。`sample.rkt`が 1631 行の C のコードにコンパイルされている。

```sh
wc main.c
#  1631  2607 45829 main.c
```

さらに、`m-eval.rkt`は 4.1 節のメタ循環評価器を、練習問題 5.51 の処理系で実行できるように多少変更したものである。

次のようにすることで、メタ循環評価器をコンパイルし、その上で`sample.rkt`を実行できる。

```sh
./compile.sh m-eval.rkt; ./main sample.rkt
# 1.000000
# quote_value
# 2.000000
# 3.000000
# ...
```

`main.c`の行数をカウントする。`m-eval.rkt`が 8334 行の C のコードにコンパイルされている。

```sh
wc main.c
#  8334  13665 247755 main.c
```
