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
   (define 
     (fib n)
     (if (< n 2) 
       n
       (+ (fib (- n 1)) (fib (- n 2)))
     )
   )
   (fib 0)
   (fib 1)
   (fib 2)
   (fib 3)
   (fib 4)
   (fib 5)
   (fib 6)
   (fib 7)
   (fib 8)
   (fib 9)
   ; 0
   ; {total-pushes = 16 maximum-depth = 8}
   ; 1
   ; {total-pushes = 16 maximum-depth = 8}
   ; 1
   ; {total-pushes = 72 maximum-depth = 13}
   ; 2
   ; {total-pushes = 128 maximum-depth = 18}
   ; 3
   ; {total-pushes = 240 maximum-depth = 23}
   ; 5
   ; {total-pushes = 408 maximum-depth = 28}
   ; 8
   ; {total-pushes = 688 maximum-depth = 33}
   ; 13
   ; {total-pushes = 1136 maximum-depth = 38}
   ; 21
   ; {total-pushes = 1864 maximum-depth = 43}
   ; 34
   ; {total-pushes = 3040 maximum-depth = 48}
  )
)

(start eceval)

; a. Fib(n)を計算するのに必要なスタックの最大深度は5n + 3である。

; b. Fib(n)を計算するときのスタックのPushの総数をS(n)とすると

; S(0) = 16
; S(1) = 16
; S(2) = S(0) + S(1) + 40 = 72
; S(3) = S(1) + S(2) + 40 = 128
; S(4) = S(2) + S(3) + 40 = 240
; S(5) = S(3) + S(4) + 40 = 408

; となる。したがってkは40。問題分より、S(n)はa * Fib(n) + bと表せるから、次の連立方程式を解く。

; S(2) = a * Fib(3) + b = 72
; S(3) = a * Fib(4) + b = 128

; 2a + b = 72
; 3a + b = 128

; b = 72 - 2a = 128 - 3a
; 72 - 2a = 128 - 3a
; a = 128 - 72 = 56
; b = 72 - 2a = 72 - 112 = -40

; したがって、S(n) = 56 * Fib(n + 1) - 40
