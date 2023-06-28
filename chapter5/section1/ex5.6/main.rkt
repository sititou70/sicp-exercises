#lang racket
(provide (all-defined-out))

(require sicp)
(require "machine/make-machine.rkt")

; main
(define 
  fib-machine
  (make-machine 
    '(n val continue)
    (list 
      (list '< <)
      (list '+ +)
      (list '- -)
      (list '* *)
    )
    '(controller
      (assign continue (label fib-done))

      fib-loop
      (test (op <) (reg n) (const 2))
      (branch (label immediate-answer))

      ;; Fib(n-1) を求める準備
      (save continue)
      (assign continue (label afterfib-n-1))
      (save n) ; n の古い値を保存
      (assign n (op -) (reg n) (const 1)) ; n を n-1 で上書き
      (goto (label fib-loop)) ; 再帰呼び出しの実⾏

      afterfib-n-1 ; リターン時に Fib(n-1) は val に⼊っている
      (restore n)
      ;(restore continue) 不要な処理

      ;; Fib(n-2) を求める準備
      (assign n (op -) (reg n) (const 2))
      ;(save continue) 不要な処理
      (assign continue (label afterfib-n-2))
      (save val) ; Fib(n-1) を保存
      (goto (label fib-loop))

      afterfib-n-2 ; リターン時に Fib(n-2) は val に⼊っている
      (assign n (reg val)) ; n には Fib(n-2) が⼊る
      (restore val) ; val には Fib(n-1) が⼊る
      (restore continue)
      (assign 
        val ; Fib(n-1) + Fib(n-2)
        (op +)
        (reg val)
        (reg n)
      )
      (goto (reg continue)) ; 呼び出し元に戻る、答えはvalの中

      immediate-answer
      (assign val (reg n)) ; 基底の場合: Fib(n) = n
      (goto (reg continue))

      fib-done
     )
  )
)

(set-register-contents! fib-machine 'n 10)
(start fib-machine)
(get-register-contents fib-machine 'val)
; 55
