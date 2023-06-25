#lang racket/base

(define 
  (square x)
  (* x x)
)

(define 
  (expmod base exp m)
  (cond 
    ((= exp 0) 1)
    ((even? exp)
     (remainder 
       (square (expmod base (/ exp 2) m))
       m
     )
    )
    (else
     (remainder 
       (* base (expmod base (- exp 1) m))
       m
     )
    )
  )
)

; nが素数なら#tを返す
(define 
  (fermat-test-for-all n)

  (define 
    (try-it a)
    (= (expmod a n n) a)
  )

  (define 
    (iter a)
    (if (= a 0) 
      #t
      (if (try-it a) 
        (iter (- a 1))
        #f
      )
    )
  )

  (iter (- n 1))
)

; カーマイケル数．いずれも#tを表示するが素数ではない
(fermat-test-for-all 561)
(fermat-test-for-all 1105)
(fermat-test-for-all 1729)
(fermat-test-for-all 2465)
(fermat-test-for-all 2821)
(fermat-test-for-all 6601)
