#lang racket
(provide (all-defined-out))

(require sicp)
(require "machine/make-machine.rkt")

; main
(define 
  machine
  (make-machine 
    (list 
      (list '< <)
      (list '+ +)
      (list '- -)
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
      (restore continue)

      ;; Fib(n-2) を求める準備
      (assign n (op -) (reg n) (const 2))
      (save continue)
      (assign continue (label afterfib-n-2))
      (save val) ; Fib(n-1) を保存
      (goto (label fib-loop))

      afterfib-n-2 ; リターン時に Fib(n-2) は val に⼊っている
      ; ここで、valからsaveしたFib(n-1)をnに復元する
      (restore n)
      ; 以下はオリジナルの処理
      ;(assign n (reg val)) ; n には Fib(n-2) が⼊る
      ;(restore val) ; val には Fib(n-1) が⼊る
      (restore continue)
      (assign 
        val ; Fib(n-2) + Fib(n-1)
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

(set-register-contents! machine 'n 10)
(start machine)
(get-register-contents machine 'val)
; 55
