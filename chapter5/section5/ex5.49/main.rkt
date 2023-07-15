#lang racket
(provide (all-defined-out))

(require sicp)
(require "machine/make-machine.rkt")
(require "machine/assembler/assemble.rkt")
(require "compiler/compile.rkt")
(require "compiler/repl.rkt")

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
  compiler-repl
  (make-machine 
    '(env val proc argl continue)
    (append 
      (list 
        (list 'read read-inputs)
        (list 'prompt-for-input (lambda (_) '()))
      )
      repl-operations
      (list 
        (list 
          'assemble
          (lambda (controller-text) 
            (assemble 
              controller-text
              compiler-repl
            )
          )
        )
      )
      compile-operations
    )
    repl-texts
  )
)

(set! 
  inputs
  '
  (
   ; 一通りの言語機能についてテストする

   ; self-evaluating
   true
   false
   1
   "2"
   ; #t
   ; #f
   ; 1
   ; 2

   ; quote
   'quote-value
   '3
   ; quote-value
   ; 3

   ; definition, variable assignment
   (define a 4)
   a
   (set! a 5)
   a
   ; 4
   ; 5

   ; if
   (if true 6)
   (if false 999 7)
   ; 6
   ; 7

   ; lambda, application
   ((lambda (a) a) 8)
   (define (nine) 9)
   (nine)
   ; 8
   ; 9

   ; begin
   (begin 
     (set! a 10)
     a
   )
   ; 10

   ; cond
   (cond 
     (false 999)
     ((= a 10) 11)
   )
   (cond 
     (false 999)
     (else 12)
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

(start compiler-repl)
