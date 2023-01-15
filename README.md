# sicp-exercises

計算機プログラムの構造と解釈（SICP）の練習問題を解いたもの

- [真鍋訳：SICP 非公式日本語版 翻訳改訂版](http://vocrf.net/docs_ja/jsicp.pdf)
- [和田訳：計算機プログラムの構造と解釈](https://sicp.iijlab.net/fulltext/xcont.html)
- [minghai 訳：SICP 非公式日本語版](https://github.com/minghai/sicp-pdf/blob/japanese/jsicp.pdf)

## 環境構築

Ubuntu + VS Code を想定します．

`sudo apt install gauche rlwrap`を実行します．

.zshrc などがあれば以下を追記します．

```sh
## scheme (gache)
### install: sudo apt install gauche rlwrap
### uninstall: sudo apt remove gauche rlwrap
alias gosh='rlwrap gosh -i -I "."'
```

VS Code で，ワークスペースで推奨している拡張機能を導入します．

syntax ハイライトや保存時のフォーマットが有効になります．

## 実行方法

REPL を起動

```sh
gosh
```

ファイルを実行

```sh
gosh hoge.scm
```
