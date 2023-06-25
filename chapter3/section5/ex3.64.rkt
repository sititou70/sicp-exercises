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
  (stream-map proc s)
  (if (stream-null? s) 
    the-empty-stream
    (cons-stream 
      (proc (stream-car s))
      (stream-map proc (stream-cdr s))
    )
  )
)

; main
(define (average x y) (/ (+ x y) 2))
(define 
  (sqrt-improve guess x)
  (average guess (/ x guess))
)
(define 
  (sqrt-stream x)
  (define 
    guesses
    (cons-stream 
      1.0
      (stream-map 
        (lambda (guess) (sqrt-improve guess x))
        guesses
      )
    )
  )
  guesses
)

(define 
  (stream-limit s tolerance)
  (define 
    (iter s value)
    (let 
      ( ;
       (new-value (stream-car s))
      )

      (if (< (abs (- value new-value)) tolerance) 
        new-value
        (iter (stream-cdr s) new-value)
      )
    )
  )
  (iter (stream-cdr s) (stream-car s))
)

(define 
  (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance)
)

(sqrt 2 0.01)
; 1.4142156862745097
