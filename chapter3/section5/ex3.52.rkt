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
  (stream-ref s n)
  (if (= n 0) 
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))
  )
)

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

(define 
  (display-stream s)
  (define 
    (display-line x)
    (display "display: ")
    (displayln x)
  )
  (stream-for-each display-line s)
)

(define 
  (even? n)
  (= (remainder n 2) 0)
)

; main
(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define 
  seq
  (stream-map 
    accum
    (stream-enumerate-interval 1 20)
  )
)
(define y (stream-filter even? seq))
(define 
  z
  (stream-filter 
    (lambda (x) (= (remainder x 5) 0))
    seq
  )
)

(stream-ref y 7)
; 136
; （6, 10, 28, 36, 66, 78, 120, 136と得られたところで止まる）
sum
; 136

(display-stream z)
; display: 10
; display: 15
; display: 45
; display: 55
; display: 105
; display: 120
; display: 190
; display: 210
sum
; 210
; display-streamがすべての要素を計算するので、1〜20の合計になる
