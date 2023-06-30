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
              (let 
                ( ;
                 (next-insts (get-contents pc))
                )

                (if (null? next-insts) 
                  'done
                  (let 
                    ( ;
                     (next-inst (car next-insts))
                    )

                    (if (instruction-break? next-inst) 
                      (begin 
                        (display "breaked ")
                        (display (instruction-label next-inst))
                        (display ":")
                        (display (+ (instruction-position next-inst) 1))
                        (display " ")
                        (displayln (instruction-text next-inst))
                      )
                      (execute)
                    )
                  )
                )
              )
            )
          )
        )
      )

      (define 
        (set-breakpoint-mode label-name pos break?)
        (for-each 
          (lambda (inst) 
            (if 
              (and 
                (eq? (instruction-label inst) label-name)
                (= (instruction-position inst) (- pos 1))
              )
              (set-instruction-break! inst break?)
            )
          )
          the-instruction-sequence
        )
      )
      (define 
        (set-all-breakpoint-mode break?)
        (for-each 
          (lambda (inst) 
            (set-instruction-break! inst break?)
          )
          the-instruction-sequence
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
          ((eq? message 'set-breakpoint)
           (lambda (label-name pos) 
             (set-breakpoint-mode label-name pos true)
           )
          )
          ((eq? message 'cancel-breakpoint)
           (lambda (label-name pos) 
             (set-breakpoint-mode label-name pos false)
           )
          )
          ((eq? message 'cancel-all-breakpoint)
           (lambda () 
             (set-all-breakpoint-mode false)
           )
          )
          (else
           (error 
             "Unknown request: MACHINE"
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
(define 
  (set-breakpoint machine label-name pos)
  ((machine 'set-breakpoint) label-name pos)
)
(define 
  (cancel-breakpoint machine label-name pos)
  ((machine 'cancel-breakpoint) label-name pos)
)
(define 
  (cancel-all-breakpoint machine)
  ((machine 'cancel-all-breakpoint))
)
