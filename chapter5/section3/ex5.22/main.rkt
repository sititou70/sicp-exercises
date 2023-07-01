#lang racket
(provide (all-defined-out))

(require sicp)
(require "machine/make-machine.rkt")

; main
(define 
  machine-a
  (make-machine 
    '(x y tmp result continue)
    (list 
      (list 'null? null?)
      (list 'cons cons)
      (list 'car car)
      (list 'cdr cdr)
    )
    '( ;
      ; 最終的にgotoするアドレスを指定
      (assign continue (label done))

      append
      (test (op null?) (reg x))
      (branch (label immediate-y))

      (save x)
      (assign x (op cdr) (reg x))
      (save continue)
      (assign continue (label append-for-cdr-done))
      (goto (label append))

      append-for-cdr-done
      (restore continue)
      (restore x)
      (assign tmp (op car) (reg x))
      (assign result (op cons) (reg tmp) (reg result))
      (goto (reg continue))

      immediate-y
      (assign result (reg y))
      (goto (reg continue))

      done
     )
  )
)

(set-register-contents! machine-a 'x '(1 2 3))
(set-register-contents! machine-a 'y '(4 5 6))
(start machine-a)
(displayln 
  (get-register-contents machine-a 'result)
)
; {1 2 3 4 5 6}

(define 
  machine-b
  (make-machine 
    '(x y tmp result continue)
    (list 
      (list 'null? null?)
      (list 'cons cons)
      (list 'car car)
      (list 'cdr cdr)
      (list 'set-car! set-car!)
      (list 'set-cdr! set-cdr!)
    )
    '( ;
      append
      (assign result (reg x))
      (assign continue (label last-pair-done))
      (goto (label last-pair))

      last-pair-done
      (perform (op set-cdr!) (reg result) (reg y))
      (goto (label done))

      ; resultのリストの最後のペアを求める
      last-pair
      (assign tmp (op cdr) (reg result))
      (test (op null?) (reg tmp))
      (branch (label immediate-result))

      (assign result (op cdr) (reg result))
      (goto (label last-pair))

      immediate-result
      (goto (reg continue))

      done
     )
  )
)

(set-register-contents! machine-b 'x '(1 2 3))
(set-register-contents! machine-b 'y '(4 5 6))
(start machine-b)
(displayln 
  (get-register-contents machine-b 'x)
)
; {1 2 3 4 5 6}
