#lang racket/base

(define 
  (make-rand)

  (define a 4086618881)
  (define b 6945494267)
  (define m 8321946851)
  (define x 0)

  (define 
    (rand c)
    (cond 
      ((eq? c 'generate)
       (set! x (remainder (+ (* a x) b) m))
       x
      )
      ((eq? c 'reset)
       (lambda (new-x) (set! x new-x))
      )
      (else
       (error "Unknown request : MAKE-RAND" c)
      )
    )
  )
  rand
)

(define rand (make-rand))

((rand 'reset) 123)
(rand 'generate)
; 1960858719
(rand 'generate)
; 7125103636
(rand 'generate)
; 4544539760

((rand 'reset) 123)
(rand 'generate)
; 1960858719
(rand 'generate)
; 7125103636
(rand 'generate)
; 4544539760
