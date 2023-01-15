(define 
  (good-enough? prev-guess guess x)
  (if (boolean? prev-guess) 
    #f
    (< (abs (- prev-guess guess)) (* guess 0.001))
  )
)

(define 
  (improve guess x)
  (/ 
    (+ 
      (/ 
        x
        (* guess guess)
      )
      (* 2 guess)
    )
    3
  )
)

(define 
  (curt-iter prev-guess guess x)
  (if 
    (or 
      (not (good-enough? prev-guess guess x))
      (boolean? prev-guess)
    )
    (curt-iter guess (improve guess x) x)
    guess
  )
)

(define 
  (curt x)
  (curt-iter #f 1.0 x)
)

(print (curt (* 10 10 10)))
; 10.000000145265767

(print (curt 10))
; 2.154434691772293
