#lang racket/base
; 以下のように実行する
; racket ex1.44.scm > ex1.44.md

(define 
  (compose f g)
  (lambda (x) (f (g x)))
)

(define 
  (repeated f n)

  (cond 
    ((= n 1) f)
    ((> n 1) (compose f (repeated f (- n 1))))
    (else (error "Argument n must be positive and non-zero value."))
  )
)

(define 
  (smooth dx)

  (lambda (f) 
    (lambda (x) 
      (/ 
        (+ 
          (f (- x dx))
          (f x)
          (f (+ x dx))
        )
        3
      )
    )
  )
)

(define 
  (plot f from dx times)

  (define 
    (plot-value times)

    (define x (+ from (* dx times)))

    (display "{\"x\":")
    (display x)
    (display ",\"f(x)\":")
    (display (f x))
    (display "}")
    (display 
      (if (> times 0) 
        ","
        ""
      )
    )

    (if (> times 0) 
      (plot-value (- times 1))
      null
    )
  )

  (display 
    "
```vega-lite
{
  \"width\": 500,
  \"height\": 500,
  \"encoding\": {
    \"x\": {\"field\": \"x\", \"type\": \"quantitative\"},
    \"y\": {\"field\": \"f(x)\", \"type\": \"quantitative\"}
  },
  \"mark\": \"line\",
  \"data\": {\"values\": ["
  )
  (plot-value times)
  (display 
    "]}
}
```
"
  )
)

(display "f(x)：\n\n")
(define 
  (f x)
  (+ 
    (sin x)
    (* (cos (* x 20)) 0.1)
    (* (sin (* x 30)) 0.1)
    (* (cos (* x 50)) 0.1)
    (* (sin (* x 70)) 0.1)
    (* (cos (* x 110)) 0.1)
    (* (sin (* x 130)) 0.1)
  )
)
(plot f -3 0.02 300)

(define smooth-dx 0.07)

(display "f(x)の1回平滑化：\n\n")
(plot ((repeated (smooth smooth-dx) 1) f) -3 0.01 600)

(display "f(x)の2回平滑化：\n\n")
(plot ((repeated (smooth smooth-dx) 2) f) -3 0.01 600)

(display "f(x)の3回平滑化：\n\n")
(plot ((repeated (smooth smooth-dx) 3) f) -3 0.01 600)

(display "f(x)の4回平滑化：\n\n")
(plot ((repeated (smooth smooth-dx) 4) f) -3 0.01 600)

(display "f(x)の5回平滑化：\n\n")
(plot ((repeated (smooth smooth-dx) 5) f) -3 0.01 600)
