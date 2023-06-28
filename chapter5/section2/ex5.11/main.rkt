#lang racket
(provide (all-defined-out))

(require sicp)
(require 
  (only-in "machine-a/make-machine.rkt" [make-machine make-machine-a] 
           [set-register-contents! set-register-contents!-a] [get-register-contents 
           get-register-contents-a] [start start-a]
  )
)
(require 
  (only-in "machine-b/make-machine.rkt" [make-machine make-machine-b] [start start-b])
)
(require 
  (only-in "machine-c/make-machine.rkt" [make-machine make-machine-c] 
           [get-register-contents get-register-contents-c] [start start-c]
  )
)

; main
(define 
  machine-a
  (make-machine-a 
    '(n val continue)
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
(set-register-contents!-a machine-a 'n 10)
(start-a machine-a)
(get-register-contents-a machine-a 'val)
; 55

(define 
  machine-b
  (make-machine-b 
    '(a b)
    (list)
    '( ;
      (assign a (const 1))
      (assign b (const 2))

      (save a)
      (save b)
      (restore b)
      (restore a)

      (save b)
      (save a)
      (restore b)
      (restore a)
     )
  )
)
; (start-b machine-b)
; エラーになる
; Source and destination registers must be the same (mcons (mcons 'poped-item (mcons (mcons 'a 1) '())) (mcons (mcons 'restore-destination (mcons 'b '())) '()))

(define 
  machine-c
  (make-machine-c 
    '(a b)
    (list)
    '( ;
      (assign a (const 1))
      (assign b (const 2))

      (save a)
      (save b)

      (assign a (const 123))

      (restore a)
     )
  )
)
(start-c machine-c)
(get-register-contents-c machine-c 'a)
; 1
