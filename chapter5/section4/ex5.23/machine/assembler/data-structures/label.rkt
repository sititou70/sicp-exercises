#lang racket
(provide (all-defined-out))

(require sicp)

; ラベル：処理の列のエントリポイント、処理の部分列へのポインタとして表される
(define 
  (make-label-entry label-name insts)
  (cons label-name insts)
)
