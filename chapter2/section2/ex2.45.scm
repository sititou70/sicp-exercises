#lang racket/base
; このプログラムは必要な詳細の手続きをまだ定義していないため動作しない。

(define 
  (split first-split second-split)
  (lambda (painter n) 
    (if (= n 0) 
      painter
      (let 
        ((smaller (split painter (- n 1))))
        (first-split painter (second-split smaller smaller))
      )
    )
  )
)

(define right-split (split beside below))
(define up-split (split below beside))
