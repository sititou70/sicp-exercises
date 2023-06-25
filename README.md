# sicp-exercises

計算機プログラムの構造と解釈（SICP）の練習問題を解いたもの

- [真鍋訳：SICP 非公式日本語版 翻訳改訂版](http://vocrf.net/docs_ja/jsicp.pdf)
- [和田訳：計算機プログラムの構造と解釈](https://sicp.iijlab.net/fulltext/xcont.html)
- [minghai 訳：SICP 非公式日本語版](https://github.com/minghai/sicp-pdf/blob/japanese/jsicp.pdf)
- [原文](https://mitp-content-server.mit.edu/books/content/sectbyfn/books_pres_0/6515/sicp.zip/full-text/book/book-Z-H-4.html)

## 環境構築

想定環境：Ubuntu + VS Code

```sh
sudo apt install racket libssl-dev
raco pkg install racket-langserver
raco pkg install sicp
```

VSCode を起動し、ワークスペースで推奨している拡張機能を導入します。

### フォーマッターの有効化

`autodesk.autolispext`をフォーマッターとして採用していますが、この拡張機能は標準だと`autolisp`しかフォーマットしません。

そこで、`./enable-formatter.sh`を実行して拡張機能を書き換え、フォーマッターを`racket`でも利用可能にします。

書き換えられた拡張機能は、`./uninstall-formatter.sh`によって削除したあと再度インストールすることでもとに戻ります。

## 実行方法

REPL を起動

```sh
racket
```

ファイルを実行

```sh
racket hoge.rkt
```
