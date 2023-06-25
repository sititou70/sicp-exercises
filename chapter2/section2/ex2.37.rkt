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
  (accumulate-n op init seqs)
  (if (null? (car seqs)) 
    null
    (cons 
      (accumulate 
        op
        init
        (map car seqs)
      )
      (accumulate-n 
        op
        init
        (map cdr seqs)
      )
    )
  )
)

(define 
  (dot-product v w)
  (accumulate + 0 (map * v w))
)
(dot-product (list 1 2 3) (list 4 5 6))
; 32

(define 
  (matrix-*-vector m v)
  (map 
    (lambda (row) 
      (accumulate 
        +
        0
        (map * row v)
      )
    )
    m
  )
)
(matrix-*-vector 
  (list 
    (list 1 2 3)
    (list 4 5 6)
    (list 7 8 9)
  )
  (list 1 2 3)
)
; (14 32 50)

(define 
  (transpose mat)
  (accumulate-n cons null mat)
)
(transpose 
  (list 
    (list 1 2 3)
    (list 4 5 6)
    (list 7 8 9)
  )
)
; ((1 4 7) (2 5 8) (3 6 9))

(define 
  (matrix-*-matrix m n)
  (let 
    ((cols (transpose n)))
    (map 
      (lambda (row) 
        (map 
          (lambda (col) 
            (accumulate 
              +
              0
              (accumulate-n 
                *
                1
                (list row col)
              )
            )
          )
          cols
        )
      )
      m
    )
  )
)
(matrix-*-matrix 
  (list 
    (list 1 2 3)
    (list 4 5 6)
    (list 7 8 9)
  )
  (list 
    (list 7 4 1)
    (list 8 5 2)
    (list 9 6 3)
  )
)
; ((50 32 14) (122 77 32) (194 122 50))
