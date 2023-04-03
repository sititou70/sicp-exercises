#lang racket/base

(define 
  (enumerate-interval low high)
  (if (> low high) 
    null
    (cons low (enumerate-interval (+ low 1) high))
  )
)

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
  (flatmap proc seq)
  (accumulate append null (map proc seq))
)

(define 
  (unique-triples n)
  (flatmap 
    (lambda (i) 
      (flatmap 
        (lambda (j) 
          (map 
            (lambda (k) (list i j k))
            (enumerate-interval 1 (- j 1))
          )
        )
        (enumerate-interval 1 (- i 1))
      )
    )
    (enumerate-interval 1 n)
  )
)

(define 
  (triple-sums n s)
  (map 
    (lambda (triple) 
      (list 
        (car triple)
        (cadr triple)
        (caddr triple)
        (+ 
          (car triple)
          (cadr triple)
          (caddr triple)
        )
      )
    )
    (filter 
      (lambda (triple) 
        (= s 
           (+ 
             (car triple)
             (cadr triple)
             (caddr triple)
           )
        )
      )
      (unique-triples n)
    )
  )
)

(triple-sums 6 10)
; '((5 3 2 10) (5 4 1 10) (6 3 1 10))
