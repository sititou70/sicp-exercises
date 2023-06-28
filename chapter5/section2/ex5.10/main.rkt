#lang racket
(provide (all-defined-out))

(require sicp)
(require "machine/make-machine.rkt")

; main
(define 
  machine
  (make-machine 
    '(a b)
    (list)
    '( ;
      (assign a (const 1))
      (assign b (const 2))
      ; 2つのレジスタの値を交換するswap命令を追加した
      ; 問題文が言及している節の構文手続きであるexpressions.rktとmake-execution-procedure.rkt以外を変更していない
      (swap a b)
     )
  )
)

(start machine)
(get-register-contents machine 'a)
; 2
(get-register-contents machine 'b)
; 1
