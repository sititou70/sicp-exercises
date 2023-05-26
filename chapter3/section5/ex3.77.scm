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
  (stream-ref s n)
  (if (= n 0) 
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))
  )
)

; main
(define 
  (integral delayed-integrand initial-value dt)
  (cons-stream 
    initial-value
    (let 
      ( ;
       (integrand (force delayed-integrand))
      )

      (if (stream-null? integrand) 
        the-empty-stream
        (integral 
          (delay (stream-cdr integrand))
          (+ (* dt (stream-car integrand)) 
             initial-value
          )
          dt
        )
      )
    )
  )
)

(define 
  (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y
)

(stream-ref 
  (solve (lambda (y) y) 1 0.00001)
  100000
)
; 2.7182682371744953
