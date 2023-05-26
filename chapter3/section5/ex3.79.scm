#lang racket

; from: https://stackoverflow.com/questions/13998388/running-code-from-sicp-section-3-5-4-with-drracket
(define the-empty-stream '())
(define 
  (stream-null? stream)
  (null? stream)
)
(define-syntax 
  cons-stream
  (syntax-rules 
    ()
    ((cons-stream head tail) 
      (cons head (delay tail))
    )
  )
)
(define 
  (stream-car stream)
  (car stream)
)
(define 
  (stream-cdr stream)
  (force (cdr stream))
)

; utils
(define 
  (stream-map proc . argstreams)
  (if (stream-null? (car argstreams)) 
    the-empty-stream
    (cons-stream 
      (apply proc (map stream-car argstreams))
      (apply stream-map 
             (cons proc (map stream-cdr argstreams))
      )
    )
  )
)

(define (add-streams s1 s2) (stream-map + s1 s2))

(define 
  (scale-stream stream factor)
  (stream-map 
    (lambda (x) (* x factor))
    stream
  )
)

(define 
  (integral delayed-integrand initial-value dt)
  (define 
    int
    (cons-stream 
      initial-value
      (let 
        ((integrand (force delayed-integrand)))
        (add-streams (scale-stream integrand dt) int)
      )
    )
  )
  int
)

(define 
  (stream-ref s n)
  (if (= n 0) 
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))
  )
)

; main
(define 
  (solve-2nd f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y
)

(define dt 0.0001)
(define t 1)
(stream-ref 
  (solve-2nd (lambda (dy y) (* dy y)) 0 1 dt)
  (/ t dt)
)

; 理論値：sqrt(2) tan(t/sqrt(2))
; 参考：https://www.wolframalpha.com/input?i=y%27%27%3Dy%27y%2C+y%280%29%3D0%2Cy%27%280%29%3D1&lang=ja
(* 
  (sqrt 2)
  (tan (/ t (sqrt 2)))
)
