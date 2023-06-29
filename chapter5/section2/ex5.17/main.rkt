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

      (perform (op trace-on))
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
      (perform (op trace-off))
      (perform (op display) (reg global-n))
      (perform (op display) (const ": "))
      (perform (op print-and-reset-exec-counter))

      (test (op <) (const 2) (reg global-n))
      (branch (label done))
      (assign global-n (op +) (reg global-n) (const 1))
      (goto (label global-loop))

      done
     )
  )
)

(start fact-machine)
; fact-loop: {test {op =} {reg n} {const 1}}
; fact-loop: {branch {label base-case}}
; base-case: {assign val {const 1}}
; base-case: {goto {reg continue}}
; fact-done: {perform {op trace-off}}
; 1: 12
; fact-loop: {test {op =} {reg n} {const 1}}
; fact-loop: {branch {label base-case}}
; fact-loop: {save continue}
; fact-loop: {save n}
; fact-loop: {assign n {op -} {reg n} {const 1}}
; fact-loop: {assign continue {label after-fact}}
; fact-loop: {goto {label fact-loop}}
; fact-loop: {test {op =} {reg n} {const 1}}
; fact-loop: {branch {label base-case}}
; base-case: {assign val {const 1}}
; base-case: {goto {reg continue}}
; after-fact: {restore n}
; after-fact: {restore continue}
; after-fact: {assign val {op *} {reg n} {reg val}}
; after-fact: {goto {reg continue}}
; fact-done: {perform {op trace-off}}
; 2: 26
; fact-loop: {test {op =} {reg n} {const 1}}
; fact-loop: {branch {label base-case}}
; fact-loop: {save continue}
; fact-loop: {save n}
; fact-loop: {assign n {op -} {reg n} {const 1}}
; fact-loop: {assign continue {label after-fact}}
; fact-loop: {goto {label fact-loop}}
; fact-loop: {test {op =} {reg n} {const 1}}
; fact-loop: {branch {label base-case}}
; fact-loop: {save continue}
; fact-loop: {save n}
; fact-loop: {assign n {op -} {reg n} {const 1}}
; fact-loop: {assign continue {label after-fact}}
; fact-loop: {goto {label fact-loop}}
; fact-loop: {test {op =} {reg n} {const 1}}
; fact-loop: {branch {label base-case}}
; base-case: {assign val {const 1}}
; base-case: {goto {reg continue}}
; after-fact: {restore n}
; after-fact: {restore continue}
; after-fact: {assign val {op *} {reg n} {reg val}}
; after-fact: {goto {reg continue}}
; after-fact: {restore n}
; after-fact: {restore continue}
; after-fact: {assign val {op *} {reg n} {reg val}}
; after-fact: {goto {reg continue}}
; fact-done: {perform {op trace-off}}
; 3: 37
; 'done
