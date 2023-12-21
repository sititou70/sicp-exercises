#lang racket

(begin 
  (define 
    (fib n)
    (if (< n 2) 
      n
      (+ 
        (fib (- n 1))
        (fib (- n 2))
      )
    )
  )
  (displayln (fib 27))
  ; 196418
)
