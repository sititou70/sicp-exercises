#lang racket
(provide (all-defined-out))

(require sicp)
(require "data-structures/register.rkt")
(require "data-structures/stack.rkt")
(require "data-structures/instruction.rkt")

; basic machine model
(define 
  (make-new-machine)
  (let 
    ( ;
     (pc (make-register 'pc)) ; プログラムカウンタ：現在実行している命令の参照
     (flag (make-register 'flag))
     (stack (make-stack))
     (the-instruction-sequence '())
    )

    (let 
      ( ;
       (the-ops 
         (list 
           (list 
             'initialize-stack
             (lambda () (stack 'initialize))
           )
         )
       )
       (register-table 
         (list 
           (list 'pc pc)
           (list 'flag flag)
         )
       )
      )
      (define 
        (allocate-register name)
        (if (assoc name register-table) 
          (error "Multiply defined register:" name)
          (set! 
            register-table
            (cons (list name (make-register name)) 
                  register-table
            )
          )
        )
        'register-allocated
      )
      (define 
        (lookup-register name)
        (let 
          ( ;
           (val (assoc name register-table))
          )

          (if val 
            (cadr val)
            (error "Unknown register:" name)
          )
        )
      )
      (define 
        (execute)
        (let 
          ( ;
           (insts (get-contents pc))
          )

          (if (null? insts) 
            'done
            (begin 
              ((instruction-execution-proc (car insts)))
              (execute)
            )
          )
        )
      )
      (define 
        (dispatch message)
        (cond 
          ((eq? message 'start)
           (set-contents! pc the-instruction-sequence)
           (execute)
          )
          ((eq? message 'install-instruction-sequence)
           (lambda (seq) 
             (set! the-instruction-sequence seq)
           )
          )
          ((eq? message 'allocate-register)
           allocate-register
          )
          ((eq? message 'get-register)
           lookup-register
          )
          ((eq? message 'install-operations)
           (lambda (ops) 
             (set! the-ops (append the-ops ops))
           )
          )
          ((eq? message 'stack) stack)
          ((eq? message 'operations) the-ops)
          (else
           (error 
             " Unknown request : MACHINE "
             message
           )
          )
        )
      )
      dispatch
    )
  )
)

(define 
  (get-register machine reg-name)
  ((machine 'get-register) reg-name)
)
