#lang racket
(provide (all-defined-out))

(require sicp)
(require "machine/make-machine.rkt")

; main
(define 
  machine
  (make-machine 
    '(a)
    (list 
      (list 'displayln displayln)
    )
    '( ;
      test
      (perform (op displayln) (label test))
     )
  )
)
; ASSENBLE: operands must registers or constants: (mcons 'label (mcons 'test '()))
