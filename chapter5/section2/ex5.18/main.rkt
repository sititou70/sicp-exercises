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
      (list 'displayln displayln)
    )
    '(controller
      (assign global-n (const 1))

      global-loop
      (assign n (reg global-n))
      (assign continue (label fact-done)) ;set up final return address

      (perform (op trace-reg) (const n))
      (perform (op trace-reg) (const val))
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
      (perform (op untrace-reg) (const n))
      (perform (op untrace-reg) (const val))

      (perform (op display) (reg global-n))
      (perform (op display) (const "! = "))
      (perform (op displayln) (reg val))
      (perform (op displayln) (const "################"))

      (test (op <) (const 5) (reg global-n))
      (branch (label done))
      (assign global-n (op +) (reg global-n) (const 1))
      (goto (label global-loop))

      done
     )
  )
)

(start fact-machine)
; val: *unassigned* -> 1
; 1! = 1
; ################
; n: 2 -> 1
; val: 1 -> 1
; n: 1 -> 2
; val: 1 -> 2
; 2! = 2
; ################
; n: 3 -> 2
; n: 2 -> 1
; val: 2 -> 1
; n: 1 -> 2
; val: 1 -> 2
; n: 2 -> 3
; val: 2 -> 6
; 3! = 6
; ################
; n: 4 -> 3
; n: 3 -> 2
; n: 2 -> 1
; val: 6 -> 1
; n: 1 -> 2
; val: 1 -> 2
; n: 2 -> 3
; val: 2 -> 6
; n: 3 -> 4
; val: 6 -> 24
; 4! = 24
; ################
; n: 5 -> 4
; n: 4 -> 3
; n: 3 -> 2
; n: 2 -> 1
; val: 24 -> 1
; n: 1 -> 2
; val: 1 -> 2
; n: 2 -> 3
; val: 2 -> 6
; n: 3 -> 4
; val: 6 -> 24
; n: 4 -> 5
; val: 24 -> 120
; 5! = 120
; ################
; n: 6 -> 5
; n: 5 -> 4
; n: 4 -> 3
; n: 3 -> 2
; n: 2 -> 1
; val: 120 -> 1
; n: 1 -> 2
; val: 1 -> 2
; n: 2 -> 3
; val: 2 -> 6
; n: 3 -> 4
; val: 6 -> 24
; n: 4 -> 5
; val: 24 -> 120
; n: 5 -> 6
; val: 120 -> 720
; 6! = 720
; ################
; 'done
