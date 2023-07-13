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
  (displayln (get-register-contents machine 'val))
  (machine 'print-stack-statistics)
)

; コンパイル版
(define 
  (compile-and-start-fib n)
  (make-and-run-machine 
    (statements 
      (compile 
        `
        (begin 
          (define 
            (fib n)
            (if (< n 2) 
              n
              (+ (fib (- n 1)) 
                 (fib (- n 2))
              )
            )
          )
          (fib ,n)
        )
        'val
        'next
      )
    )
  )
)
(compile-and-start-fib 2)
(compile-and-start-fib 3)
(compile-and-start-fib 4)
(compile-and-start-fib 5)
(compile-and-start-fib 6)
(compile-and-start-fib 7)
(compile-and-start-fib 8)
(compile-and-start-fib 9)
(compile-and-start-fib 10)
(compile-and-start-fib 20)
(compile-and-start-fib 25)
(compile-and-start-fib 30)
; 1
; {total-pushes = 12 maximum-depth = 5}
; 2
; {total-pushes = 22 maximum-depth = 8}
; 3
; {total-pushes = 42 maximum-depth = 11}
; 5
; {total-pushes = 72 maximum-depth = 14}
; 8
; {total-pushes = 122 maximum-depth = 17}
; 13
; {total-pushes = 202 maximum-depth = 20}
; 21
; {total-pushes = 332 maximum-depth = 23}
; 34
; {total-pushes = 542 maximum-depth = 26}
; 55
; {total-pushes = 882 maximum-depth = 29}
; 6765
; {total-pushes = 109452 maximum-depth = 59}
; 75025
; {total-pushes = 1213922 maximum-depth = 74}
; 832040
; {total-pushes = 13462682 maximum-depth = 89}

; 特別目的マシン
(define 
  (run-special-fib-machine n)

  (define 
    machine
    (make-machine 
      '(n val continue)
      (list 
        (list '< <)
        (list '- -)
        (list '+ +)
      )
      '
      ( ;
       controller
       (assign continue (label fib-done))

       fib-loop
       (test (op <) (reg n) (const 2))
       (branch (label immediate-answer))
       ;; Fib(n  1) を求める準備
       (save continue)
       (assign continue (label afterfib-n-1))
       (save n) ; n の古い値を保存
       (assign n (op -) (reg n) (const 1)) ; n を n-1 で上書き
       (goto (label fib-loop)) ; 再帰呼び出しの実⾏

       afterfib-n-1 ; リターン時に Fib(n  1) は val に⼊っている
       (restore n)
       (restore continue)
       ;; Fib(n  2) を求める準備
       (assign n (op -) (reg n) (const 2))
       (save continue)
       (assign continue (label afterfib-n-2))
       (save val) ; Fib(n  1) を保存
       (goto (label fib-loop))

       afterfib-n-2 ; リターン時に Fib(n  2) は val に⼊っている
       (assign n (reg val)) ; n には Fib(n  2) が⼊る
       (restore val) ; val には Fib(n  1) が⼊る
       (restore continue)
       (assign 
         val ; Fib(n  1) + Fib(n  2)
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

  (set-register-contents! machine 'n n)
  (start machine)
  (displayln (get-register-contents machine 'val))
  (machine 'print-stack-statistics)
)
(run-special-fib-machine 2)
(run-special-fib-machine 3)
(run-special-fib-machine 4)
(run-special-fib-machine 5)
(run-special-fib-machine 6)
(run-special-fib-machine 7)
(run-special-fib-machine 8)
(run-special-fib-machine 9)
(run-special-fib-machine 10)
(run-special-fib-machine 20)
(run-special-fib-machine 25)
(run-special-fib-machine 30)
; 1
; {total-pushes = 4 maximum-depth = 2}
; 2
; {total-pushes = 8 maximum-depth = 4}
; 3
; {total-pushes = 16 maximum-depth = 6}
; 5
; {total-pushes = 28 maximum-depth = 8}
; 8
; {total-pushes = 48 maximum-depth = 10}
; 13
; {total-pushes = 80 maximum-depth = 12}
; 21
; {total-pushes = 132 maximum-depth = 14}
; 34
; {total-pushes = 216 maximum-depth = 16}
; 55
; {total-pushes = 352 maximum-depth = 18}
; 6765
; {total-pushes = 43780 maximum-depth = 38}
; 75025
; {total-pushes = 485568 maximum-depth = 48}
; 832040
; {total-pushes = 5385072 maximum-depth = 58}

; 結果のまとめはnote.mdに書いた
