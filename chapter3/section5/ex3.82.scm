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
  (partial-sums s)
  (add-streams s (cons-stream 0 (partial-sums s)))
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
  (estimate-integral-stream p x1 x2 y1 y2)

  (define 
    (random-in-range low high)
    (+ low (* high (random)))
  )

  (define 
    (random-point-stream)
    (cons-stream 
      (list (random-in-range x1 x2) (random-in-range y1 y2))
      (random-point-stream)
    )
  )

  (define 
    hits-sum-stream
    (partial-sums 
      (stream-map 
        (lambda (point) 
          (if (apply p point) 1 0)
        )
        (random-point-stream)
      )
    )
  )

  (define ones (cons-stream 1 ones))
  (define integers (cons-stream 1 (add-streams ones integers)))

  (stream-map 
    (lambda (hits trials) 
      (* 
        (* (- x2 x1) (- y2 y1))
        (/ hits trials)
      )
    )
    hits-sum-stream
    integers
  )
)

(stream-ref 
  (estimate-integral-stream 
    (lambda (x y) 
      (< (+ (* x x) (* y y)) 1)
    )
    -1.0
    1.0
    -1.0
    1.0
  )
  10000
)
; 3.1192880711928805
