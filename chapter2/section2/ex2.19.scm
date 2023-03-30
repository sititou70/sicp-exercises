#lang racket/base

(define 
  (cc amount coin-values)

  (define (no-more? coin-values) (null? coin-values))

  (define (except-first-denomination coin-values) (cdr coin-values))

  (define (first-denomination coin-values) (car coin-values))

  (cond 
    ((= amount 0) 1)
    ((or (< amount 0) (no-more? coin-values)) 0)
    (else
     (+ 
       (cc 
         amount
         (except-first-denomination 
           coin-values
         )
       )
       (cc 
         (- amount 
            (first-denomination 
              coin-values
            )
         )
         coin-values
       )
     )
    )
  )
)

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)
; 292

; コインの順番は返される解答に影響を与えない
; この手続きは、両替に各コインを使う場合とそうでない場合を分けて処理しており、その場合分けの順番は結果に影響しないからである。
(cc 100 (list 50 25 10 5 1))
; 292
(cc 100 (list 1 5 10 25 50))
; 292
(cc 100 (list 25 1 5 50 10))
; 292
