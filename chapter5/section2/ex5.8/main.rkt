#lang racket
(provide (all-defined-out))

(require sicp)
(require "machine/make-machine.rkt")
(require "machine/assembler/assemble.rkt")

; main
(define 
  machine
  (make-machine 
    '(a)
    (list)
    '( ;
      start
      (goto (label here))

      here
      (assign a (const 3))
      (goto (label there))

      here
      (assign a (const 4))
      (goto (label there))

      there
     )
  )
)

; extract-labelが生成するlabelsでは、instructionの出現順と同順にlabelsが格納されている
; これは、extract-labelの以下の処理が再帰的であり、next-instをconsの第1引数に指定していることからもわかる

; (cons 
;   (make-label-entry 
;     next-inst
;     insts
;   )
;   labels
; )

; gotoでは、lookup-label手続きがlabelsを探索してジャンプ先を決定するが、これは単なるassocであるから1つ目のhereが常に見つかる
; したがって、最終的なレジスタaの中身は3となる
(start machine)
(get-register-contents machine 'a)
; 3

; labelの名前が一意でない場合エラーを発生させるようにする
(use-strict-extract-labels)
(define 
  machine2
  (make-machine 
    '(a)
    (list)
    '( ;
      start
      (goto (label here))

      here
      (assign a (const 3))
      (goto (label there))

      here
      (assign a (const 4))
      (goto (label there))

      there
     )
  )
)
; labels must have unique name: 'here
