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
  (smooth s)
  (stream-map 
    (lambda (v1 v2) (/ (+ v1 v2) 2))
    s
    (stream-cdr s)
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
  (make-zero-crossings s)
  (let 
    ( ;
     (smoothed (smooth s))
    )

    (stream-map 
      sign-change-detector
      smoothed
      (stream-cdr smoothed)
    )
  )
)

(print-stream-nth (make-zero-crossings sense-data) 32)
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
; [10] 0
; [11] 0
; [12] 0
; [13] 0
; [14] 0
; [15] 0
; [16] 0
; [17] 0
; [18] 0
; [19] 0
; [20] 1
; [21] 0
; [22] 0
; [23] 0
; [24] 0
; [25] 0
; [26] 0
; [27] 0
; [28] 0
; [29] 0
; [30] 0
; [31] 0
; ノイズのセロ交差は含まれない
