#lang racket
(provide (all-defined-out))

(require sicp)
(require "machine/make-machine.rkt")
(require "compiler/environment.rkt")
(require "compiler/compile.rkt")
(require "compiler/instruction-sequence.rkt")
(require "compiler/global-environment.rkt")

; main
(define 
  (make-and-run-machine insts)

  (define 
    machine
    (make-machine 
      '(exp env val proc argl continue)
      compile-operations
      insts
    )
  )

  (set-register-contents! machine 'env (setup-environment))
  (start machine)
)

(define 
  test1
  '
  (begin 
    ; 一通りの言語機能についてテストする

    ; self-evaluating
    (displayln true)
    (displayln false)
    (displayln 1)
    (displayln "2")
    ; #t
    ; #f
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
(define 
  insts1
  (statements 
    (compile 
      test1
      'val
      'next
      (extend-compile-time-environment 
        (make-compile-time-frame the-empty-environment)
        '()
      )
    )
  )
)
(make-and-run-machine insts1)

; 本文中の例
(define 
  test2
  '
  (displayln 
    (((lambda (x y) 
        (lambda (a b c d e) 
          ((lambda (y z) (* x y z)) 
            (* a b x)
            (+ c d x)
          )
        )
      ) 
       3
       0
     ) 
      1
      2
      3
      4
      5
    )
  )
  ; 180
)
(define 
  insts2
  (statements 
    (compile 
      test2
      'val
      'next
      (extend-compile-time-environment 
        (make-compile-time-frame the-empty-environment)
        '()
      )
    )
  )
)

; コンパイル結果を表示する。結果にレキシカルアドレスが含まれているのがわかる
(map displayln insts2)
; {assign proc {op lookup-variable-value} {const displayln} {reg env}}
; {save proc}
; {assign proc {op make-compiled-procedure} {label entry183} {reg env}}
; {goto {label after-lambda184}}
; 
; entry183
; {assign env {op compiled-procedure-env} {reg proc}}
; {assign env {op extend-environment} {const {x y}} {reg argl} {reg env}}
; {assign val {op make-compiled-procedure} {label entry185} {reg env}}
; {goto {reg continue}}
; 
; entry185
; {assign env {op compiled-procedure-env} {reg proc}}
; {assign env {op extend-environment} {const {a b c d e}} {reg argl} {reg env}}
; {assign proc {op make-compiled-procedure} {label entry187} {reg env}}
; {goto {label after-lambda188}}
; 
; entry187
; {assign env {op compiled-procedure-env} {reg proc}}
; {assign env {op extend-environment} {const {y z}} {reg argl} {reg env}}
; {assign proc {op lookup-variable-value} {const *} {reg env}}
; {assign val {op lexical-address-lookup} {const {0 . 1}} {reg env}}
; {assign argl {op list} {reg val}}
; {assign val {op lexical-address-lookup} {const {0 . 0}} {reg env}}
; {assign argl {op cons} {reg val} {reg argl}}
; {assign val {op lexical-address-lookup} {const {2 . 0}} {reg env}}
; {assign argl {op cons} {reg val} {reg argl}}
; {test {op primitive-procedure?} {reg proc}}
; {branch {label primitive-branch189}}
; 
; compiled-branch190
; {assign val {op compiled-procedure-entry} {reg proc}}
; {goto {reg val}}
; 
; primitive-branch189
; {assign val {op apply-primitive-procedure} {reg proc} {reg argl}}
; {goto {reg continue}}
; 
; after-call191
; after-lambda188
; {save continue}
; {save proc}
; {save env}
; {assign proc {op lookup-variable-value} {const +} {reg env}}
; {assign val {op lexical-address-lookup} {const {1 . 0}} {reg env}}
; {assign argl {op list} {reg val}}
; {assign val {op lexical-address-lookup} {const {0 . 3}} {reg env}}
; {assign argl {op cons} {reg val} {reg argl}}
; {assign val {op lexical-address-lookup} {const {0 . 2}} {reg env}}
; {assign argl {op cons} {reg val} {reg argl}}
; {test {op primitive-procedure?} {reg proc}}
; {branch {label primitive-branch195}}
; 
; compiled-branch196
; {assign continue {label after-call197}}
; {assign val {op compiled-procedure-entry} {reg proc}}
; {goto {reg val}}
; 
; primitive-branch195
; {assign val {op apply-primitive-procedure} {reg proc} {reg argl}}
; 
; after-call197
; {assign argl {op list} {reg val}}
; {restore env}
; {save argl}
; {assign proc {op lookup-variable-value} {const *} {reg env}}
; {assign val {op lexical-address-lookup} {const {1 . 0}} {reg env}}
; {assign argl {op list} {reg val}}
; {assign val {op lexical-address-lookup} {const {0 . 1}} {reg env}}
; {assign argl {op cons} {reg val} {reg argl}}
; {assign val {op lexical-address-lookup} {const {0 . 0}} {reg env}}
; {assign argl {op cons} {reg val} {reg argl}}
; {test {op primitive-procedure?} {reg proc}}
; {branch {label primitive-branch192}}
; 
; compiled-branch193
; {assign continue {label after-call194}}
; {assign val {op compiled-procedure-entry} {reg proc}}
; {goto {reg val}}
; 
; primitive-branch192
; {assign val {op apply-primitive-procedure} {reg proc} {reg argl}}
; 
; after-call194
; {restore argl}
; {assign argl {op cons} {reg val} {reg argl}}
; {restore proc}
; {restore continue}
; {test {op primitive-procedure?} {reg proc}}
; {branch {label primitive-branch198}}
; 
; compiled-branch199
; {assign val {op compiled-procedure-entry} {reg proc}}
; {goto {reg val}}
; 
; primitive-branch198
; {assign val {op apply-primitive-procedure} {reg proc} {reg argl}}
; {goto {reg continue}}
; 
; after-call200
; after-lambda186
; after-lambda184
; {assign val {const 0}}
; {assign argl {op list} {reg val}}
; {assign val {const 3}}
; {assign argl {op cons} {reg val} {reg argl}}
; {test {op primitive-procedure?} {reg proc}}
; {branch {label primitive-branch201}}
; 
; compiled-branch202
; {assign continue {label proc-return204}}
; {assign val {op compiled-procedure-entry} {reg proc}}
; {goto {reg val}}
; 
; proc-return204
; {assign proc {reg val}}
; {goto {label after-call203}}
; 
; primitive-branch201
; {assign proc {op apply-primitive-procedure} {reg proc} {reg argl}}
; 
; after-call203
; {assign val {const 5}}
; {assign argl {op list} {reg val}}
; {assign val {const 4}}
; {assign argl {op cons} {reg val} {reg argl}}
; {assign val {const 3}}
; {assign argl {op cons} {reg val} {reg argl}}
; {assign val {const 2}}
; {assign argl {op cons} {reg val} {reg argl}}
; {assign val {const 1}}
; {assign argl {op cons} {reg val} {reg argl}}
; {test {op primitive-procedure?} {reg proc}}
; {branch {label primitive-branch205}}
; 
; compiled-branch206
; {assign continue {label after-call207}}
; {assign val {op compiled-procedure-entry} {reg proc}}
; {goto {reg val}}
; 
; primitive-branch205
; {assign val {op apply-primitive-procedure} {reg proc} {reg argl}}
; 
; after-call207
; {assign argl {op list} {reg val}}
; {restore proc}
; {test {op primitive-procedure?} {reg proc}}
; {branch {label primitive-branch208}}
; 
; compiled-branch209
; {assign continue {label after-call210}}
; {assign val {op compiled-procedure-entry} {reg proc}}
; {goto {reg val}}
; 
; primitive-branch208
; {assign val {op apply-primitive-procedure} {reg proc} {reg argl}}
; 
; after-call210

(make-and-run-machine insts2)
