#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "m-eval/eval-apply.scm")
(require "m-eval/global-environment.scm")

; main
; 練習問題4.6で実装したletを使った派生式としてlet*を定義すれば十分と思われる。

(eval 
  '(let
    ( ;
     (x 1)
     (y (+ 1 1))
    )

    (displayln "hello let")
    (+ (* y y) x)
   )
  the-global-environment
)
; hello let
; 5

(eval 
  '(let*
    ( ;
     (x 3)
     (y (+ x 2))
     (z (+ x y 5))
    )

    (displayln "hello let*")
    (* x z)
   )
  the-global-environment
)
; hello let*
; 39

(eval 
  '(define
    (fib n)
    (let 
      fib-iter
      ( ;
       (a 1)
       (b 0)
       (count n)
      )

      (displayln b)
      (if (= count 0) 
        b
        (fib-iter (+ a b) a (- count 1))
      )
    )
   )
  the-global-environment
)
; 'ok
(eval '(fib 9) the-global-environment)
; 0
; 1
; 1
; 2
; 3
; 5
; 8
; 13
; 21
; 34
; 34
