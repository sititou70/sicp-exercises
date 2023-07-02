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

     (define 
       (iter product counter)
       (if (> counter n) 
         product
         (iter (* counter product) (+ counter 1))
       )
     )

     (iter 1 1)
   )
   (factorial 1)
   ; 1
   ; {total-pushes = 64 maximum-depth = 10}
   (factorial 2)
   ; 2
   ; {total-pushes = 99 maximum-depth = 10}
   (factorial 3)
   ; 6
   ; {total-pushes = 134 maximum-depth = 10}
   (factorial 4)
   ; 24
   ; {total-pushes = 169 maximum-depth = 10}
   (factorial 5)
   ; 120
   ; {total-pushes = 204 maximum-depth = 10}
   (factorial 6)
   ; 720
   ; {total-pushes = 239 maximum-depth = 10}
   (factorial 7)
   ; 5040
   ; {total-pushes = 274 maximum-depth = 10}
   (factorial 8)
   ; 40320
   ; {total-pushes = 309 maximum-depth = 10}
   (factorial 9)
   ; 362880
   ; {total-pushes = 344 maximum-depth = 10}
  )
)

(start eceval)

; a. n!の計算に必要なスタックの深さはnから独立しており、10である
; b. n!の計算に必要なpushの回数は35n + 29で表される
