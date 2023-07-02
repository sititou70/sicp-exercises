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
   ; quote-value
   '3
   ; 3

   ; definition, variable assignment
   (define a 4)
   a
   ; 4
   (set! a 5)
   a
   ; 5

   ; if
   (if true 6)
   ; 6
   (if false 999 7)
   ; 7

   ; lambda, application
   ((lambda (a) a) 8)
   ; 8
   (define (nine) 9)
   (nine)
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
   ; 11
   (cond 
     (false 999)
     (else 12)
   )
   ; 12

   ; let
   (let 
     ( ;
      (a 13)
     )

     a
   )
   ; 13
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
   ; 14

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

   ; 遅延評価に関するテスト
   ;; 第2引数は遅延されるためゼロ除算エラーは発生しない
   ((lambda (x y) x) (+ 1 2) (/ 3 0))
   ; 3

   ;; xの定義で、f1の呼び出しに、その時点では定義されていないyを使用しているが、正規順序評価であるため問題ない
   (define 
     (f)
     (define (f1 x) x)

     (define x (f1 y))
     (define y 1)
     x
   )
   (f)
   ; 1

   ;; 遅延リストによって自然対数の底を近似してみる
   (define (cons x y) (lambda (m) (m x y)))
   (define (car z) (z (lambda (p q) p)))
   (define (cdr z) (z (lambda (p q) q)))
   (define 
     (list-ref items n)
     (if (= n 0) 
       (car items)
       (list-ref (cdr items) (- n 1))
     )
   )
   (define 
     (map proc items)
     (if (null? items) 
       '()
       (cons 
         (proc (car items))
         (map proc (cdr items))
       )
     )
   )
   (define 
     (scale-list items factor)
     (map (lambda (x) (* x factor)) items)
   )
   (define 
     (add-lists list1 list2)
     (cond 
       ((null? list1) list2)
       ((null? list2) list1)
       (else
        (cons 
          (+ (car list1) (car list2))
          (add-lists (cdr list1) (cdr list2))
        )
       )
     )
   )
   (define 
     (integral integrand initial-value dt)
     (define 
       int
       (cons 
         initial-value
         (add-lists (scale-list integrand dt) int)
       )
     )
     int
   )
   (define 
     (solve f y0 dt)
     (define y (integral dy y0 dt))
     (define dy (map f y))
     y
   )
   (list-ref (solve (lambda (x) x) 1 0.0001) 10000)
   ; 2.7181459268252266
  )
)

(start eceval)
