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
   ; この練習問題で初めてecevalを導入するため、一通りの言語機能についてテストする

   ; self-evaluating
   true
   false
   1
   "2"

   ; quote
   'quote-value
   '3

   ; definition, variable assignment
   (define a 4)
   a
   (set! a 5)
   a

   ; if
   (if true 6)
   (if false 999 7)

   ; lambda, application
   ((lambda (a) a) 8)
   (define (nine) 9)
   (nine)

   ; begin
   (begin 
     (set! a 10)
     a
   )

   ; cond
   (cond 
     (false 999)
     ((= a 10) 11)
   )
   (cond 
     (false 999)
     (else 12)
   )

   ; let
   (let 
     ( ;
      (a 13)
     )

     a
   )
   (let 
     ( ;
      (a 1)
      (b 2)
      (c 3)
     )

     (let 
       ( ;
        (b 10)
       )

       (+ a b c)
     )
   )

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
  )
)

(start eceval)
; #t
; #f
; 1
; 2
; quote-value
; 3
; ok
; 4
; ok
; 5
; 6
; 7
; 8
; ok
; 9
; 10
; 11
; 12
; 13
; 14
; ok
; {a b c d e f}
; ok
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
; no more input
