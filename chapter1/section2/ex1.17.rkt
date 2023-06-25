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
  (* a b)
  (if (= b 1) 
    a
    (if (even? b) 
      (* (double a) (halve b))
      (+ a (* a (+ b -1)))
    )
  )
)

(trace *)
(* 123 997)
