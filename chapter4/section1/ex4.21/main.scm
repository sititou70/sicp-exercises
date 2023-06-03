#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "m-eval/eval-apply.scm")
(require "m-eval/global-environment.scm")

; main
; 10の階乗を計算する
(eval 
  '((lambda (n) 
      ((lambda (fact) (fact fact n)) 
        (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))
      )
    )
    10
   )
  the-global-environment
)
; 3628800

; 15番目のフィボナッチ数を求める
(eval 
  '((lambda (n) 
      ((lambda (fib) (fib fib n)) 
        (lambda (f k) 
          (cond 
            ((= k 0) 0)
            ((= k 1) 1)
            (else
             (+ 
               (f f (- k 1))
               (f f (- k 2))
             )
            )
          )
        )
      )
    )
    15
   )
  the-global-environment
)
; 610

(eval 
  '(define
    (f x)
    ((lambda (even? odd?) (even? even? odd? x)) 
      (lambda (ev? od? n) 
        (if (= n 0) true (od? ev? od? (- n 1)))
      )
      (lambda (ev? od? n) 
        (if (= n 0) false (ev? ev? od? (- n 1)))
      )
    )
   )
  the-global-environment
)

(eval '(f 10) the-global-environment)
; #t
(eval '(f 11) the-global-environment)
; #f
(eval '(f 12) the-global-environment)
; #t
(eval '(f 13) the-global-environment)
; #f
