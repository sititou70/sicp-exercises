# sicp-exercises

計算機プログラムの構造と解釈（SICP）の練習問題を解いたもの

- [真鍋訳：SICP 非公式日本語版 翻訳改訂版](http://vocrf.net/docs_ja/jsicp.pdf)
- [和田訳：計算機プログラムの構造と解釈](https://sicp.iijlab.net/fulltext/xcont.html)
- [minghai 訳：SICP 非公式日本語版](https://github.com/minghai/sicp-pdf/blob/japanese/jsicp.pdf)

## 環境構築

Ubuntu + VS Code を想定します．

`sudo apt install racket`を実行します．

VS Code で，ワークスペースで推奨している拡張機能を導入します．

syntax ハイライトや保存時のフォーマットが有効になります．

### `#lang sicp`をサポート

例えば、3 章以降に登場する`set-car!`などのサポートに必要です。

```sh
sudo apt install libssl-dev
raco pkg install sicp
```

## 実行方法

REPL を起動

```sh
racket
```

ファイルを実行

```sh
racket hoge.scm
```
