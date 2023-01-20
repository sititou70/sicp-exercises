#lang racket/base

; 再帰的なf
(define 
  (recursive-f n)
  (if (< n 3) 
    n
    (+ 
      (recursive-f (- n 1))
      (* 2 (recursive-f (- n 2)))
      (* 3 (recursive-f (- n 3)))
    )
  )
)

(recursive-f 5)
(recursive-f 10)
(recursive-f 20)
(recursive-f 35)

; 手続き的なf
(define 
  (liner-f n)

  (define 
    ; a: f(cnt - 1)
    ; b: f(cnt - 2)
    ; c: f(cnt - 3)
    (iter cnt a b c)
    (if (> cnt n) 
      a
      (iter 
        (+ cnt 1)
        (+ a (* 2 b) (* 3 c))
        a
        b
      )
    )
  )

  (if (< n 3) 
    n
    (iter 3 2 1 0)
  )
)

(liner-f 5)
(liner-f 10)
(liner-f 20)
(liner-f 35)
