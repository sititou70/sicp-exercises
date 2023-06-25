#lang racket/base
; このプログラムはup-splitの実装を示すのみで、必要な詳細の手続きをまだ定義していないため動作しない。

(define 
  (right-split painter n)
  (if (= n 0) 
    painter
    (let 
      ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller))
    )
  )
)

(define 
  (up-split painter n)
  (if (= n 0) 
    painter
    (let 
      ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller))
    )
  )
)
