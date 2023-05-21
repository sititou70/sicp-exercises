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

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define 
  partial-sums
  (cons-stream 1 (add-streams (stream-cdr integers) partial-sums))
)

(print-stream-nth partial-sums 10)
; [0] 1
; [1] 3
; [2] 6
; [3] 10
; [4] 15
; [5] 21
; [6] 28
; [7] 36
; [8] 45
; [9] 55
