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
     (factorial-rec n)
     (if (= n 1) 1 (* (factorial-rec (- n 1)) n))
   )
   (factorial-rec 1)
   (factorial-rec 2)
   (factorial-rec 3)
   (factorial-rec 4)
   (factorial-rec 5)
   (factorial-rec 6)
   (factorial-rec 7)
   (factorial-rec 8)
   (factorial-rec 9)
   ; 1
   ; {total-pushes = 18 maximum-depth = 11}
   ; 2
   ; {total-pushes = 52 maximum-depth = 19}
   ; 6
   ; {total-pushes = 86 maximum-depth = 27}
   ; 24
   ; {total-pushes = 120 maximum-depth = 35}
   ; 120
   ; {total-pushes = 154 maximum-depth = 43}
   ; 720
   ; {total-pushes = 188 maximum-depth = 51}
   ; 5040
   ; {total-pushes = 222 maximum-depth = 59}
   ; 40320
   ; {total-pushes = 256 maximum-depth = 67}
   ; 362880
   ; {total-pushes = 290 maximum-depth = 75}

   (define 
     (factorial-iter n)
     (define 
       (iter product counter)
       (if (> counter n) 
         product
         (iter (* counter product) (+ counter 1))
       )
     )
     (iter 1 1)
   )
   (factorial-iter 1)
   (factorial-iter 2)
   (factorial-iter 3)
   (factorial-iter 4)
   (factorial-iter 5)
   (factorial-iter 6)
   (factorial-iter 7)
   (factorial-iter 8)
   (factorial-iter 9)
   ; 1
   ; {total-pushes = 70 maximum-depth = 17}
   ; 2
   ; {total-pushes = 107 maximum-depth = 20}
   ; 6
   ; {total-pushes = 144 maximum-depth = 23}
   ; 24
   ; {total-pushes = 181 maximum-depth = 26}
   ; 120
   ; {total-pushes = 218 maximum-depth = 29}
   ; 720
   ; {total-pushes = 255 maximum-depth = 32}
   ; 5040
   ; {total-pushes = 292 maximum-depth = 35}
   ; 40320
   ; {total-pushes = 329 maximum-depth = 38}
   ; 362880
   ; {total-pushes = 366 maximum-depth = 41}
  )
)

(start eceval)

; 末尾再帰が無効な場合、n!の計算に必要なスタックの最大深度およびpushの回数は以下のようになる

; |              | 最大深度 | push の総数 |
; | :----------: | :------: | :---------: |
; | 階乗（再帰） |  8n + 3  |  34n - 16   |
; | 階乗（反復） | 3n + 14  |  37n + 33   |
