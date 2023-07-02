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
     (factorial n)
     (if (= n 1) 1 (* (factorial (- n 1)) n))
   )
   (factorial 1)
   ; 1
   ; {total-pushes = 16 maximum-depth = 8}
   (factorial 2)
   ; 2
   ; {total-pushes = 48 maximum-depth = 13}
   (factorial 3)
   ; 6
   ; {total-pushes = 80 maximum-depth = 18}
   (factorial 4)
   ; 24
   ; {total-pushes = 112 maximum-depth = 23}
   (factorial 5)
   ; 120
   ; {total-pushes = 144 maximum-depth = 28}
   (factorial 6)
   ; 720
   ; {total-pushes = 176 maximum-depth = 33}
   (factorial 7)
   ; 5040
   ; {total-pushes = 208 maximum-depth = 38}
   (factorial 8)
   ; 40320
   ; {total-pushes = 240 maximum-depth = 43}
   (factorial 9)
   ; 362880
   ; {total-pushes = 272 maximum-depth = 48}
  )
)

(start eceval)

; n!の計算に必要なスタックの最大深度およびpushの回数は以下のようになる

; |              | 最大深度 | push の総数 |
; | :----------: | :------: | :---------: |
; | 階乗（再帰） |  5n + 3  |  32n - 16   |
; | 階乗（反復） |    10    |  35n + 29   |
