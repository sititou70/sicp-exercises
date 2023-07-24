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

(define 
  test
  '
  (begin 
    ; この練習問題で初めてコンパイラを導入するため、一通りの言語機能についてテストする

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
(make-and-run-machine (statements (compile test 'val 'next)))

(define 
  problem

  '(begin
    ; case1
    ; 演算子のenv: 取り除ける。fは手続き適用ではないため
    ; 被演算子のenv: 取り除ける。被演算子はどれも自己評価された式であるため
    ; 被演算子のargl: 取り除ける。被演算子はどれも自己評価された式であるため
    ; 被演算子のproc: 取り除ける。被演算子はどれも自己評価された式であるため
    (f 'x 'y)
    ; {assign proc {op lookup-variable-value} {const f} {reg env}}
    ; {assign val {const y}}
    ; {assign argl {op list} {reg val}}
    ; {assign val {const x}}
    ; {assign argl {op cons} {reg val} {reg argl}}
    ; {test {op primitive-procedure?} {reg proc}}
    ; ...

    ; case2
    ; 演算子のenv: 取り除ける。fは手続き適用でありenvを変更するが、それが被演算子に必要とされていないため
    ; 被演算子のenv: 取り除ける。被演算子はどれも自己評価された式であるため
    ; 被演算子のargl: 取り除ける。被演算子はどれも自己評価された式であるため
    ; 被演算子のproc: 取り除ける。被演算子はどれも自己評価された式であるため
    ((f) 'x 'y)
    ; {assign proc {op lookup-variable-value} {const f} {reg env}}
    ; {assign argl {const ()}}
    ; {test {op primitive-procedure?} {reg proc}}
    ; ...
    ; {assign val {const y}}
    ; {assign argl {op list} {reg val}}
    ; {assign val {const x}}
    ; {assign argl {op cons} {reg val} {reg argl}}
    ; {test {op primitive-procedure?} {reg proc}}
    ; ...

    ; case3
    ; 演算子のenv: 取り除ける。fは手続き適用ではないため
    ; 被演算子のenv: 被演算子の評価順序による。評価が左から右に行われる場合、(g 'x)によって変更されるenvが、後のyの変数参照に必要であるため、envは保存されるべきである。
    ; 一方で、本文のコンパイラでは被演算子の構築は逆から行われ、yの変数参照はenvを変更しないため取り除かれる。
    ; 被演算子のargl: 被演算子の評価順による。評価が左から右に行われる場合、yの変数参照はarglを変更しないため取り除ける。
    ; 一方で、本文のコンパイラでは被演算子の評価は逆から行われ、(g 'x)はarglを変更するため取り除かれない。
    ; 被演算子のproc: 取り除けない。被演算子の手続き適用で変更されるため
    (f (g 'x) y)
    ; {assign proc {op lookup-variable-value} {const f} {reg env}}
    ; {save proc}
    ; {assign val {op lookup-variable-value} {const y} {reg env}}
    ; {assign argl {op list} {reg val}}
    ; {save argl}
    ; {assign proc {op lookup-variable-value} {const g} {reg env}}
    ; {assign val {const x}}
    ; {assign argl {op list} {reg val}}
    ; {test {op primitive-procedure?} {reg proc}}
    ; ...
    ; {restore argl}
    ; {assign argl {op cons} {reg val} {reg argl}}
    ; {restore proc}
    ; {test {op primitive-procedure?} {reg proc}}
    ; ...

    ; case3 alt
    (f y (g 'x))
    ; {assign proc {op lookup-variable-value} {const f} {reg env}}
    ; {save proc}
    ; {save env}
    ; {assign proc {op lookup-variable-value} {const g} {reg env}}
    ; {assign val {const x}}
    ; {assign argl {op list} {reg val}}
    ; {test {op primitive-procedure?} {reg proc}}
    ; ...
    ; {assign argl {op list} {reg val}}
    ; {restore env}
    ; {assign val {op lookup-variable-value} {const y} {reg env}}
    ; {assign argl {op cons} {reg val} {reg argl}}
    ; {restore proc}
    ; {test {op primitive-procedure?} {reg proc}}
    ; ...

    ; case4
    ; 演算子のenv: 取り除ける。fは手続き適用ではないため
    ; 被演算子のenv: 取り除ける。被演算子の評価順が左から右の場合、(g 'x)はenvを変更するが、'yは自己評価式であるためenvを必要としない。右から左である場合、'yは自己評価式であるためenvを変更しない。
    ; 被演算子のargl: 被演算子の評価順による。評価順が左から右の場合、'yはarglを変更しないため取り除ける。
    ; 本文の実装のように右から左の場合、(g 'x)がarglを変更するため取り除けない。
    ; 被演算子のproc: 取り除けない。被演算子の手続き適用で変更されるため
    (f (g 'x) 'y)
    ; {assign proc {op lookup-variable-value} {const f} {reg env}}
    ; {save proc}
    ; {assign val {const y}}
    ; {assign argl {op list} {reg val}}
    ; {save argl}
    ; {assign proc {op lookup-variable-value} {const g} {reg env}}
    ; {assign val {const x}}
    ; {assign argl {op list} {reg val}}
    ; {test {op primitive-procedure?} {reg proc}}
    ; ...
    ; {restore argl}
    ; {assign argl {op cons} {reg val} {reg argl}}
    ; {restore proc}
    ; {test {op primitive-procedure?} {reg proc}}
    ; ...

    ; case4 alt
    (f 'y (g 'x))
    ; {assign proc {op lookup-variable-value} {const f} {reg env}}
    ; {save proc}
    ; {assign proc {op lookup-variable-value} {const g} {reg env}}
    ; {assign val {const x}}
    ; {assign argl {op list} {reg val}}
    ; {test {op primitive-procedure?} {reg proc}}
    ; ...
    ; {assign argl {op list} {reg val}}
    ; {assign val {const y}}
    ; {assign argl {op cons} {reg val} {reg argl}}
    ; {restore proc}
    ; {test {op primitive-procedure?} {reg proc}}
    ; ...
   )
)
(map displayln (statements (compile problem 'val 'next)))
