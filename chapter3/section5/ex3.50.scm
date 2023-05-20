#lang racket/base
(require srfi/40)

(define the-empty-stream stream-null)
(define-syntax 
  cons-stream
  (syntax-rules 
    ()
    ((_ a b) (stream-cons a b))
  )
)

(define 
  (stream-enumerate-interval low high)
  (if (> low high) 
    the-empty-stream
    (cons-stream 
      low
      (stream-enumerate-interval (+ low 1) high)
    )
  )
)

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

(define 
  (display-stream s)
  (define (display-line x) (displayln x))
  (stream-for-each display-line s)
)

(display-stream 
  (stream-map 
    *
    (stream-enumerate-interval 1 10)
    (stream-enumerate-interval 2 11)
    (stream-enumerate-interval 3 12)
  )
)
; 6
; 24
; 60
; 120
; 210
; 336
; 504
; 720
; 990
; 1320
