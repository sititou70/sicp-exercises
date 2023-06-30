#lang racket
(provide (all-defined-out))

(require sicp)
(require "machine/make-machine.rkt")
(require "machine/assembler/basic-machine-model.rkt")

; main
(define 
  gcd-machine
  (make-machine 
    '(a b t)
    (list 
      (list 'rem remainder)
      (list '= =)
    )
    '( ;
      test-b
      (test (op =) (reg b) (const 0))
      (branch (label gcd-done))
      (assign t (op rem) (reg a) (reg b))
      (assign a (reg b))
      (assign b (reg t))
      (goto (label test-b))

      gcd-done
     )
  )
)

; ブレークポイントを設定していないとき、マシンは通常通り終了する
(set-register-contents! gcd-machine 'a 206)
(set-register-contents! gcd-machine 'b 40)
(start gcd-machine)
(get-register-contents gcd-machine 'a)
; 2

; ブレークポイントを使ってみる
; aへのassignとgotoの直前でブレークする
(set-register-contents! gcd-machine 'a 206)
(set-register-contents! gcd-machine 'b 40)
(set-breakpoint gcd-machine 'test-b 4)
(set-breakpoint gcd-machine 'test-b 6)

; 実行開始
(start gcd-machine)
; breaked test-b:4 {assign a {reg b}}

; aへのassignのブレークポイントを削除
(cancel-breakpoint gcd-machine 'test-b 4)

; 実行継続
(proceed-machine gcd-machine)
; breaked test-b:6 {goto {label test-b}}

; aとbの値を表示して実行継続
(define 
  (print-and-proceed)
  (displayln 
    (list 
      (list 
        'a
        (get-register-contents gcd-machine 'a)
      )
      (list 
        'b
        (get-register-contents gcd-machine 'b)
      )
    )
  )
  (proceed-machine gcd-machine)
)

(print-and-proceed)
; {{a 40} {b 6}}
; breaked test-b:6 {goto {label test-b}}
(print-and-proceed)
; {{a 6} {b 4}}
; breaked test-b:6 {goto {label test-b}}
(print-and-proceed)
; {{a 4} {b 2}}
; breaked test-b:6 {goto {label test-b}}

; レジスタを書き換える
(set-register-contents! gcd-machine 'a 123456789)
(set-register-contents! gcd-machine 'b 987654321)
; ブレークポイントをすべて削除
(cancel-all-breakpoint gcd-machine)
; 実行継続
(proceed-machine gcd-machine)
; 'done
(get-register-contents gcd-machine 'a)
; 9
