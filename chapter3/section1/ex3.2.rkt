#lang racket/base

(define 
  (make-monitored f)

  (define count 0)

  (lambda (x) 
    (cond 
      ((eq? x 'how-many-calls?) count)
      ((eq? x 'reset-count) (set! count 0))
      (else
       (set! count (+ count 1))
       (f x)
      )
    )
  )
)

(define s (make-monitored sqrt))

(s 100)
; 10
(s 'how-many-calls?)
; 1

(s 10000)
; 100
(s 2)
; 1.4142135623730951
(s 'how-many-calls?)
; 3


(s 'reset-count)
(s 'how-many-calls?)
; 0

(s 3)
; 1.7320508075688772
(s 'how-many-calls?)
; 1
