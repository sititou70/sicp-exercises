#lang racket

(begin 
  ; self-evaluating
  (displayln 1)
  ; 1

  ; quote
  (displayln 'quote_value)
  (displayln '2)
  ; quote_value
  ; 2

  ; definition, variable assignment
  (define a 3)
  (displayln a)
  (set! a 4)
  (displayln a)
  ; 3
  ; 4

  ; if
  (displayln (if true 5 999))
  (displayln (if false 999 6))
  ; 5
  ; 6

  ; lambda, compound procedure application
  (displayln ((lambda (a) a) 7))
  (define (eight) 8)
  (displayln (eight))
  ; 7
  ; 8

  ; begin
  (displayln 
    (begin 
      (set! a 9)
      a
    )
  )
  ; 9

  ; cond
  (displayln 
    (cond 
      (false 999)
      ((= a 9) 10)
    )
  )
  (displayln 
    (cond 
      (false 999)
      (else 11)
    )
  )
  ; 10
  ; 11

  ; primitive procedure application
  (displayln 
    (+ 6 6)
  )
  ; 12
  (displayln 
    (cdr (cons 999 13))
  )
  ; 13

  ; 複雑な例
  (define 
    (append x y)
    (if (null? x) 
      y
      (cons 
        (car x)
        (append (cdr x) y)
      )
    )
  )
  (displayln (append '(a b c) '(d e f)))
  ; {a b c d e f}

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
  (displayln (fib 1))
  (displayln (fib 2))
  (displayln (fib 3))
  (displayln (fib 4))
  (displayln (fib 5))
  (displayln (fib 6))
  (displayln (fib 7))
  (displayln (fib 8))
  (displayln (fib 9))
  (displayln (fib 10))
  ; 1
  ; 1
  ; 2
  ; 3
  ; 5
  ; 8
  ; 13
  ; 21
  ; 34
  ; 55
)
