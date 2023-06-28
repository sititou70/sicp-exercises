#lang racket
(provide (all-defined-out))

(require sicp)
(require "data-structures/register.rkt")
(require "data-structures/stack.rkt")
(require "data-structures/instruction.rkt")
(require compatibility/mlist)

; basic machine model
(define 
  (make-new-machine)
  (let 
    ( ;
     (pc (make-register 'pc)) ; プログラムカウンタ：現在実行している命令の参照
     (flag (make-register 'flag))
     (stack (make-stack))
     (the-instruction-sequence '())
     (meta-registers-has-entry-point '())
     (meta-stacking-registers '())
     (meta-register-sources '())
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
        (add-meta-registers-has-entry-point reg-name)
        (set! 
          meta-registers-has-entry-point
          (cons reg-name meta-registers-has-entry-point)
        )
      )
      (define 
        (add-meta-stacking-registers reg-name)
        (set! 
          meta-stacking-registers
          (cons reg-name meta-stacking-registers)
        )
      )
      (define 
        (add-meta-register-sources reg-name source)
        (let 
          ( ;
           (entry (assoc reg-name meta-register-sources))
          )

          (if entry 
            (set-cdr! entry (cons source (cdr entry)))
            (set! 
              meta-register-sources
              (cons 
                (cons reg-name (cons source '()))
                meta-register-sources
              )
            )
          )
        )
      )
      (define 
        (get-meta)

        (define (uniq list) (list->mlist (remove-duplicates (mlist->list list))))
        (define 
          (msort list less-than?)
          (list->mlist 
            (sort (mlist->list list) less-than?)
          )
        )

        (list 
          (msort 
            (uniq 
              (map 
                (lambda (inst) (instruction-text inst))
                the-instruction-sequence
              )
            )
            (lambda (x y) 
              (string<? 
                (symbol->string (car x))
                (symbol->string (car y))
              )
            )
          )
          (uniq meta-registers-has-entry-point)
          (uniq meta-stacking-registers)
          (map 
            (lambda (source) 
              (cons 
                (car source)
                (uniq (cdr source))
              )
            )
            meta-register-sources
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
          ((eq? message 'add-meta-registers-has-entry-point)
           add-meta-registers-has-entry-point
          )
          ((eq? message 'add-meta-stacking-registers) add-meta-stacking-registers)
          ((eq? message 'add-meta-register-sources) add-meta-register-sources)
          ((eq? message 'get-meta) (get-meta))
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
