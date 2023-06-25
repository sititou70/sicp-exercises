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
(define (add-streams s1 s2) (stream-map + s1 s2))

(define s (cons-stream 1 (add-streams s s)))
; [1]: 1、cons-streamの第1引数が1だから
; [2]: 2、cons-streamの第2引数のcarになる。これは[1] + [1]
; [3]: 4、cons-streamの第2引数のcadrになる。これは[2] + [2]
; 以降、2のべき乗が続く

(print-stream-nth s 10)
; [0] 1
; [1] 2
; [2] 4
; [3] 8
; [4] 16
; [5] 32
; [6] 64
; [7] 128
; [8] 256
; [9] 512
; [10] 1024
