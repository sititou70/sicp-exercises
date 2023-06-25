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
; a. integrate-seriesを定義せよ
(define 
  (integrate-series s)

  (define 
    (iter s n)
    (cons-stream 
      (* (/ 1 n) (stream-car s))
      (iter (stream-cdr s) (+ n 1))
    )
  )
  (iter s 1)
)

(define ones (cons-stream 1 ones))

(define s (integrate-series ones))
(print-stream-nth s 10)
; [0] 1
; [1] 1/2
; [2] 1/3
; [3] 1/4
; [4] 1/5
; [5] 1/6
; [6] 1/7
; [7] 1/8
; [8] 1/9
; [9] 1/10

; b. sin と cos の級数を生成する方法を示せ
(define 
  exp-series
  (cons-stream 1 (integrate-series exp-series))
)
(print-stream-nth exp-series 10)
; [0] 1
; [1] 1
; [2] 1/2
; [3] 1/6
; [4] 1/24
; [5] 1/120
; [6] 1/720
; [7] 1/5040
; [8] 1/40320
; [9] 1/362880

(define 
  cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1))
)
(define sine-series (cons-stream 0 (integrate-series cosine-series)))

(print-stream-nth cosine-series 10)
; [0] 1
; [1] 0
; [2] -1/2
; [3] 0
; [4] 1/24
; [5] 0
; [6] -1/720
; [7] 0
; [8] 1/40320
; [9] 0

(print-stream-nth sine-series 10)
; [0] 0
; [1] 1
; [2] 0
; [3] -1/6
; [4] 0
; [5] 1/120
; [6] 0
; [7] -1/5040
; [8] 0
; [9] 1/362880
