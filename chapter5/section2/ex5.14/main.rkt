#lang racket
(provide (all-defined-out))

(require sicp)
(require "machine/make-machine.rkt")

; main
(define 
  fact-machine
  (make-machine 
    '(global-n n val continue)
    (list 
      (list '= =)
      (list '< <)
      (list '+ +)
      (list '- -)
      (list '* *)
      (list 'display display)
    )
    '(controller
      (assign global-n (const 1))

      global-loop
      (assign n (reg global-n))
      (assign continue (label fact-done)) ;set up final return address
      (perform (op initialize-stack))
      (perform (op display) (reg n))
      (perform (op display) (const ": "))

      fact-loop
      (test (op =) (reg n) (const 1))
      (branch (label base-case))

      ;; Set up for the recursive call by saving n and continue.
      ;; Set up continue so that the computation will continue
      ;; at after-fact when the subroutine returns.
      (save continue)
      (save n)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-fact))
      (goto (label fact-loop))

      after-fact
      (restore n)
      (restore continue)
      (assign val (op *) (reg n) (reg val)) ;val now contains n(n - 1)!
      (goto (reg continue)) ;return to caller

      base-case
      (assign val (const 1)) ;base case: 1! = 1
      (goto (reg continue)) ;return to caller

      fact-done
      (perform (op print-stack-statistics))
      (test (op <) (const 9) (reg global-n))
      (branch (label done))
      (assign global-n (op +) (reg global-n) (const 1))
      (goto (label global-loop))

      done
     )
  )
)

(start fact-machine)
; 1: {total-pushes = 0 maximum-depth = 0}
; 2: {total-pushes = 2 maximum-depth = 2}
; 3: {total-pushes = 4 maximum-depth = 4}
; 4: {total-pushes = 6 maximum-depth = 6}
; 5: {total-pushes = 8 maximum-depth = 8}
; 6: {total-pushes = 10 maximum-depth = 10}
; 7: {total-pushes = 12 maximum-depth = 12}
; 8: {total-pushes = 14 maximum-depth = 14}
; 9: {total-pushes = 16 maximum-depth = 16}
; 10: {total-pushes = 18 maximum-depth = 18}

; nの階乗を求めるとき、total-pushesおよびmaximum-depthは2*(n-1)となる
