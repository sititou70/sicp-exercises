#lang racket/base

(define 
  (rec-fib n)
  (cond 
    ((= n 0) 0)
    ((= n 1) 1)
    (else
     (+ (rec-fib (- n 1)) 
        (rec-fib (- n 2))
     )
    )
  )
)

(rec-fib 10)
(rec-fib 15)
(rec-fib 20)
(rec-fib 25)
(rec-fib 30)

(define 
  (fib n)

  (define 
    (fib-iter a b p q count)
    (cond 
      ((= count 0) b)
      ((even? count)
       (fib-iter 
         a
         b
         (+ (* p p) (* q q))
         (+ (* q q) (* 2 p q))
         (/ count 2)
       )
      )
      (else
       (fib-iter 
         (+ (* b q) (* a q) (* a p))
         (+ (* b p) (* a q))
         p
         q
         (- count 1)
       )
      )
    )
  )

  (fib-iter 1 0 0 1 n)
)

(fib 10)
(fib 15)
(fib 20)
(fib 25)
(fib 30)
