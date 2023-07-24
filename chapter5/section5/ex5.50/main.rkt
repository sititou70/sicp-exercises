#lang racket
(provide (all-defined-out))

(require sicp)
(require "machine/make-machine.rkt")
(require "compiler/compile.rkt")
(require "compiler/instruction-sequence.rkt")
(require "compiler/global-environment.rkt")
(require "m-eval.rkt")

; main
(define 
  test
  '
  '
  (begin 
    ; 一通りの言語機能について、コンパイルするメタ循環評価器上でテストする

    ; self-evaluating
    (displayln 1)
    (displayln "2")
    ; 1
    ; 2

    ; quote
    (displayln 'quote-value)
    (displayln '3)
    ; quote-value
    ; 3

    ; definition, variable assignment
    (define a 4)
    (displayln a)
    (set! a 5)
    (displayln a)
    ; 4
    ; 5

    ; if
    (displayln (if true 6))
    (displayln (if false 999 7))
    ; 6
    ; 7

    ; lambda, application
    (displayln ((lambda (a) a) 8))
    (define (nine) 9)
    (displayln (nine))
    ; 8
    ; 9

    ; begin
    (displayln 
      (begin 
        (set! a 10)
        a
      )
    )
    ; 10

    ; cond
    (displayln 
      (cond 
        (false 999)
        ((= a 10) 11)
      )
    )
    (displayln 
      (cond 
        (false 999)
        (else 12)
      )
    )
    ; 11
    ; 12

    ; 複雑な例
    (define 
      (append x y)
      (if (null? x) 
        y
        (cons 
          (car x)
          (append (cdr x) y)
        )
      )
    )
    (displayln (append '(a b c) '(d e f)))
    ; {a b c d e f}

    (define 
      (fib n)
      (if (< n 2) 
        n
        (+ 
          (fib (- n 1))
          (fib (- n 2))
        )
      )
    )
    (displayln (fib 1))
    (displayln (fib 2))
    (displayln (fib 3))
    (displayln (fib 4))
    (displayln (fib 5))
    (displayln (fib 6))
    (displayln (fib 7))
    (displayln (fib 8))
    (displayln (fib 9))
    (displayln (fib 10))
    ; 1
    ; 1
    ; 2
    ; 3
    ; 5
    ; 8
    ; 13
    ; 21
    ; 34
    ; 55
  )
)

; コンパイル
(define 
  controller-text
  (statements 
    (compile 
      (append 
        (list 'begin)
        m-eval
        `
        ((eval ,test the-global-environment))
      )
      'val
      'next
    )
  )
)
; コントローラの行数を見てみる
(length controller-text)
; 4761
; 表示してみる
; (map displayln controller-text)

; マシンをセットアップして実行
(define 
  machine
  (make-machine 
    '(env val proc argl continue)
    compile-operations
    controller-text
  )
)
(set-register-contents! machine 'env (setup-environment))
(start machine)
