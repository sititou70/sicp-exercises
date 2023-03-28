#lang racket/base

(define 
  (iterative-improve good-enough? improve)

  (define 
    (try guess)
    (let 
      ((next (improve guess)))
      (if (good-enough? guess next) 
        next
        (try next)
      )
    )
  )

  try
)

(define 
  (sqrt x)

  (define 
    (square x)
    (* x x)
  )
  (define 
    (good-enough? guess _)
    (< (abs (- (square guess) x)) 0.001)
  )

  (define 
    (average x y)
    (/ (+ x y) 2)
  )
  (define 
    (improve guess)
    (average guess (/ x guess))
  )

  ((iterative-improve 
     good-enough?
     improve
   ) 
    x
  )
)
(sqrt 2.0)
; 1.4142135623746899

(define 
  (fixed-point f first-guess)

  (define tolerance 0.00001)
  (define 
    (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance
    )
  )

  ((iterative-improve 
     close-enough?
     f
   ) 
    first-guess
  )
)
(fixed-point cos 1.0)
; 0.7390822985224024
