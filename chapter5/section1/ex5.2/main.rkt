#lang racket
(provide (all-defined-out))

(require sicp)
(require "machine/make-machine.rkt")

; main
(define 
  fact-machine
  (make-machine 
    '(n counter product)
    (list 
      (list '> >)
      (list '+ +)
      (list '* *)
    )
    '( ;
      test-counter
      (test (op >) (reg counter) (reg n))
      (branch (label fact-done))
      (assign product (op *) (reg counter) (reg product))
      (assign counter (op +) (reg counter) (const 1))
      (goto (label test-counter))
      fact-done
     )
  )
)

(set-register-contents! fact-machine 'n 10)
(set-register-contents! fact-machine 'counter 1)
(set-register-contents! fact-machine 'product 1)
(start fact-machine)
(get-register-contents fact-machine 'product)
; 3628800
