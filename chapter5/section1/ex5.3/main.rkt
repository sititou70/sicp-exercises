#lang racket
(provide (all-defined-out))

(require sicp)
(require "machine/make-machine.rkt")

; main
; good-enough?とimproveが基本命令として使えるとき：
(define 
  (square x)
  (* x x)
)
(define 
  (average x y)
  (/ (+ x y) 2.0)
)
(define 
  sqrt-machine1
  (make-machine 
    '(guess x)
    (list 
      (list 
        'good-enough?
        (lambda (guess x) 
          (< (abs (- (square guess) x)) 0.001)
        )
      )
      (list 
        'improve
        (lambda (guess x) 
          (average guess (/ x guess))
        )
      )
    )
    '( ;
      test-good-enough
      (test (op good-enough?) (reg guess) (reg x))
      (branch (label sqrt-done))
      (assign guess (op improve) (reg guess) (reg x))
      (goto (label test-good-enough))
      sqrt-done
     )
  )
)
(set-register-contents! sqrt-machine1 'x 2)
(set-register-contents! sqrt-machine1 'guess 1.0)
(start sqrt-machine1)
(get-register-contents sqrt-machine1 'guess)
; 1.4142156862745097

; good-enough?とimproveを算術演算によって展開するとき：
(define 
  sqrt-machine2
  (make-machine 
    '(guess x tmp)
    (list 
      (list '+ +)
      (list '- -)
      (list '* *)
      (list '/ /)
      (list '< <)
    )
    '( ;
      ; good-enough?
      test-good-enough
      (assign tmp (op *) (reg guess) (reg guess))
      (assign tmp (op -) (reg tmp) (reg x))

      ;; abs
      (test (op <) (const 0) (reg tmp))
      (branch (label abs-positive))
      (assign tmp (op *) (reg tmp) (const -1))
      abs-positive

      (test (op <) (reg tmp) (const 0.001))
      (branch (label sqrt-done))

      ; improve
      (assign tmp (op /) (reg x) (reg guess))
      (assign tmp (op +) (reg guess) (reg tmp))
      (assign guess (op /) (reg tmp) (const 2))
      (goto (label test-good-enough))

      sqrt-done
     )
  )
)
(set-register-contents! sqrt-machine2 'x 2)
(set-register-contents! sqrt-machine2 'guess 1.0)
(start sqrt-machine2)
(get-register-contents sqrt-machine2 'guess)
; 1.4142156862745097
