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
  (show x)
  (display "show: ")
  (displayln x)
  x
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
  x
  (stream-map 
    show
    (stream-enumerate-interval 0 10)
  )
)

(stream-ref x 5)
; show: 0
; show: 1
; show: 2
; show: 3
; show: 4
; show: 5
; 5
(stream-ref x 7)
; show: 6
; show: 7
; 7

; 本文にあるとおり、delayは特定のメモ化手続きと考えられる。
; したがって、stream-mapで使用されているcons-streamの第2引数はメモ化されていると考えられるため、(stream-ref x 7)では、5までのshowは実際には実行されず、メモ化された値が使用された。
; それ以降の6、7に関しては新たにshowが実行されたため、このような表示になった。
