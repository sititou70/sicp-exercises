#lang racket
(provide (all-defined-out))

(require sicp)
(require "basic-machine-model.rkt")
(require "make-execution-procedure.rkt")
(require "data-structures/instruction.rkt")
(require "data-structures/label.rkt")

(define 
  (assemble controller-text machine)
  (extract-labels 
    controller-text
    (lambda (insts labels) 
      (update-insts! insts labels machine)
      insts
    )
  )
)

(define 
  (extract-labels text receive)
  (if (null? text) 
    (receive '() '())
    (extract-labels 
      (cdr text)
      (lambda (insts labels) 
        (let 
          ( ;
           (next-inst (car text))
          )

          (if (symbol? next-inst) 
            (receive 
              insts
              (cons 
                (make-label-entry 
                  next-inst
                  insts
                )
                labels
              )
            )
            (receive 
              (cons (make-instruction next-inst) 
                    insts
              )
              labels
            )
          )
        )
      )
    )
  )
)

(define 
  (update-insts! insts labels machine)

  (define 
    (for-each-with-index proc list)

    (define 
      (iter list index)
      (if (null? list) 
        'done
        (begin 
          (proc (car list) index)
          (iter (cdr list) (+ index 1))
        )
      )
    )

    (iter list 0)
  )

  (let 
    ( ;
     (pc (get-register machine 'pc))
     (flag (get-register machine 'flag))
     (stack (machine 'stack))
     (ops (machine 'operations))
    )

    (for-each 
      (lambda (label-entry) 
        (for-each-with-index 
          (lambda (inst index) 
            (set-instruction-label! inst (car label-entry))
            (set-instruction-position! inst index)
          )
          (cdr label-entry)
        )
      )
      labels
    )
    (for-each 
      (lambda (inst) 
        (set-instruction-execution-proc! 
          inst
          (make-execution-procedure 
            (instruction-text inst)
            labels
            machine
            pc
            flag
            stack
            ops
          )
        )
      )
      insts
    )
  )
)
