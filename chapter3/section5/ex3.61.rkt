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
    (lambda (x) 
      (* x factor)
    )
    stream
  )
)

(define 
  (print-stream-nth s n)

  (define 
    (iter s cnt)

    (display "[")
    (display cnt)
    (display "] ")
    (displayln (stream-car s))

    (if (< cnt (- n 1)) 
      (iter (stream-cdr s) (+ cnt 1))
      'done
    )
  )

  (iter s 0)
)

; main
(define 
  (mul-series s1 s2)
  (cons-stream 
    (* (stream-car s1) (stream-car s2))
    (add-streams 
      (mul-series (stream-cdr s1) s2)
      (scale-stream (stream-cdr s2) (stream-car s1))
    )
  )
)

(define 
  (invert-unit-series s)
  (cons-stream 
    1
    (scale-stream 
      (mul-series 
        (stream-cdr s)
        (invert-unit-series s)
      )
      -1
    )
  )
)

(define x (cons-stream 1 (scale-stream x (/ 1 2))))
(print-stream-nth x 10)
; [0] 1
; [1] 1/2
; [2] 1/4
; [3] 1/8
; [4] 1/16
; [5] 1/32
; [6] 1/64
; [7] 1/128
; [8] 1/256
; [9] 1/512
; x = 0のとき2に収束

(define y (invert-unit-series x))
(print-stream-nth y 10)
; [0] 1
; [1] -1/2
; [2] 0
; [3] 0
; [4] 0
; [5] 0
; [6] 0
; [7] 0
; [8] 0
; [9] 0
; x = 0のとき1/2に収束
