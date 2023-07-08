#lang racket
(provide (all-defined-out))

(require sicp)
(require "machine/make-machine.rkt")
(require "ec-eval/repl.rkt")
(require "ec-eval/eval-apply.rkt")

; main
(define inputs '())
(define 
  (read-inputs)
  (if (null? inputs) 
    (begin (displayln "no more input") (exit))
  )
  (let 
    ( ;
     (input (car inputs))
    )

    (set! inputs (cdr inputs))
    input
  )
)
(define 
  eceval
  (make-machine 
    '(exp env val proc argl continue unev)
    (append 
      (list 
        (list 'read read-inputs)
        (list 'prompt-for-input (lambda (_) '()))
      )
      repl-operations
      eval-operations
      apply-operations
    )
    (append repl-insts eval-insts apply-insts)
  )
)

(set! 
  inputs
  '
  ( ;
   ; 演算子が記号でない場合
   ((lambda (a) a) 123)
   ; 123

   ; 演算子が記号の場合
   (define (f) 123)
   (f)
   ; 123

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
   (append '(a b c) '(d e f))
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
   (fib 1)
   (fib 2)
   (fib 3)
   (fib 4)
   (fib 5)
   (fib 6)
   (fib 7)
   (fib 8)
   (fib 9)
   (fib 10)
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

(start eceval)

; b. 評価器が認識するケースを増やしたとしても、たとえば構文解析のオーバーヘッドが残る。
; そのため、コンパイラのメリットが無くなるわけではない。このオーバーヘッドは、4.1.7節で最適化したものである。
