#lang racket/base

(define 
  (accumulate op initial sequence)
  (if (null? sequence) 
    initial
    (op 
      (car sequence)
      (accumulate op initial (cdr sequence))
    )
  )
)

(define 
  (map p sequence)
  (accumulate 
    (lambda (x y) 
      (cons 
        (p x)
        y
      )
    )
    null
    sequence
  )
)

(define 
  (count-leaves t)
  (accumulate 
    +
    0
    (map 
      (lambda (x) 
        (if (pair? x) 
          (count-leaves x)
          1
        )
      )
      t
    )
  )
)

(define x (cons (list 1 2) (list 3 4)))
(count-leaves x)
; 4
(count-leaves (list x x))
; 8