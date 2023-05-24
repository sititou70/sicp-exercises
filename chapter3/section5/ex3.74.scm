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
  (list2stream list)
  (if (null? list) 
    the-empty-stream
    (cons-stream 
      (car list)
      (list2stream (cdr list))
    )
  )
)

; 符号反転後にチャタリングノイズのようなものが含まれる
(define 
  sense-data
  (list2stream 
    '(100 100 100 100 100 100 100 100 100 100 -100 0.1 -100 0.1 -100 -100 -100 -100 
      -100 -100 -100 -100 100 -0.1 100 -0.1 100 100 100 100 100 100 100 100
     )
  )
)

(define 
  (sign-change-detector v1 v2)

  (define (minus-sign? x) (< x 0))
  (define (plus-sign? x) (not (minus-sign? x)))

  (cond 
    ((and (minus-sign? v1) (plus-sign? v2)) 1)
    ((and (plus-sign? v1) (minus-sign? v2)) -1)
    (else 0)
  )
)
(define 
  zero-crossings
  (stream-map 
    sign-change-detector
    sense-data
    (stream-cdr sense-data)
  )
)

(print-stream-nth zero-crossings 33)
; [0] 0
; [1] 0
; [2] 0
; [3] 0
; [4] 0
; [5] 0
; [6] 0
; [7] 0
; [8] 0
; [9] -1
; [10] 1
; [11] -1
; [12] 1
; [13] -1
; [14] 0
; [15] 0
; [16] 0
; [17] 0
; [18] 0
; [19] 0
; [20] 0
; [21] 1
; [22] -1
; [23] 1
; [24] -1
; [25] 1
; [26] 0
; [27] 0
; [28] 0
; [29] 0
; [30] 0
; [31] 0
; [32] 0
; ノイズのゼロ交差を検知してしまっている
