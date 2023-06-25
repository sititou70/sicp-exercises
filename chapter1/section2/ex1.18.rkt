#lang racket/base
(require racket/trace)

(define 
  (even? n)
  (= (remainder n 2) 0)
)

(define 
  (double n)
  (+ n n)
)

(define 
  (halve n)
  (/ n 2)
)

(define 
  (iter a b c)
  (if (= c 1) 
    (+ a b)
    (if (even? c) 
      (iter a (double b) (halve c))
      (iter (+ a b) b (+ c -1))
    )
  )
)

(define 
  (* a b)
  (iter 0 a b)
)

(trace iter)
(* 123 997)
