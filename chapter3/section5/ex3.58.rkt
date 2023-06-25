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
; expandは、numをdenで割ったときの、基数radixでの少数表示を計算する。
; これは、num / denを筆算で求める手続きに対応している。
(define 
  (expand num den radix)
  (cons-stream 
    (quotient (* num radix) den) ; 筆算の各桁で商を求める部分に対応
    (expand (remainder (* num radix) den) den radix) ; 筆算の各桁で余りを求める部分に対応
  )
)

; 1 / 7 = 0.142857142857...
(print-stream-nth (expand 1 7 10) 11)
; [0] 1
; [1] 4
; [2] 2
; [3] 8
; [4] 5
; [5] 7
; [6] 1
; [7] 4
; [8] 2
; [9] 8
; [10] 5
; [11] 7

; 3 / 8 = 0.37500...
(print-stream-nth (expand 3 8 10) 5)
; [0] 3
; [1] 7
; [2] 5
; [3] 0
; [4] 0
