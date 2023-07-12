#lang racket
(provide (all-defined-out))

(require sicp)
(require "machine/make-machine.rkt")
(require "compiler/compile.rkt")
(require "compiler/instruction-sequence.rkt")
(require "compiler/global-environment.rkt")

; main
(define 
  (make-and-run-machine insts)

  (define 
    machine
    (make-machine 
      '(env val proc argl continue)
      compile-operations
      insts
    )
  )

  (set-register-contents! machine 'env (setup-environment))
  (start machine)
)

; 我々のコンパイラは、組み合わせの被演算子に対して右から左の評価順を生成する。
; テスト用のプログラム
(define 
  program
  '
  (begin 
    (define (f a b c) (list a b c))
    (displayln (f 1 2 3))
  )
)
; コンパイルする
(define 
  statements1
  (statements (compile program 'val 'next))
)
; 実行してみる
(make-and-run-machine statements1)
; {1 2 3}
; 手続きの適用が正常に行えていることがわかる
(map displayln statements1)
; コンパイル結果のうち、被演算子リストの構築部分は以下のようになっている
; {assign val {const 3}}                                                                                                                                                                                 
; {assign argl {op list} {reg val}}                                                                                                                                                                      
; {assign val {const 2}}                                                                                                                                                                                 
; {assign argl {op cons} {reg val} {reg argl}}                                                                                                                                                           
; {assign val {const 1}}                                                                                                                                                                                 
; {assign argl {op cons} {reg val} {reg argl}}                                                                                                                                                           

; この評価順を決定しているのは、被演算子リストの構築部分をコンパイルする、construct-arglist-altとcode-to-get-rest-args-alt手続きである。
; 以下の手続き呼び出しは、左から右へ被演算子リストを評価するように修正されたconstruct-arglist-altとcode-to-get-rest-args-alt手続きを有効にする。
(use-different-arguments-evaluation-order)
; コンパイルする
(define 
  statements2
  (statements (compile program 'val 'next))
)
; 実行してみる
(make-and-run-machine statements2)
; {1 2 3}
; 手続きの適用が正常に行えていることがわかる
(map displayln statements2)
; コンパイル結果のうち、被演算子リストの構築部分は以下のようになっている
; {assign val {const 1}}
; {assign argl {op list} {reg val}}
; {assign val {const 2}}
; {assign val {op list} {reg val}}
; {assign argl {op append} {reg argl} {reg val}}
; {assign val {const 3}}
; {assign val {op list} {reg val}}
; {assign argl {op append} {reg argl} {reg val}}

; オリジナルのコンパイル結果では被演算子リストの先頭に評価結果をconsすれば良かったが、今回は評価順が逆であるためappendが必要になっている。
; arglの最後に要素をappendするには、arglの長さだけcdrを実行する必要があるため、オリジナルのコンパイル結果に比べて効率が悪いといえる

; 別のアイデアとしては、引数の内部表現をそもそも逆にしてしまうというものがある。
; 環境のextend手続きや基本手続きの適用手続きは、arglが逆であることを期待し、それをreverseしたうえで扱う。
; それにより、被演算子リストの構築時にappendを使用しなくても良くなる。
; しかしそれよりは、オリジナルのコンパイラの実装の方が直感的かもしれない。
