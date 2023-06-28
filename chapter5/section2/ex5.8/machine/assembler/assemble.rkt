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
  (strict-extract-labels text receive)
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
            (if (assoc next-inst labels) 
              (error "labels must have unique name:" next-inst)
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
            )
            (receive 
              (cons 
                (make-instruction next-inst)
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
  (use-strict-extract-labels)
  (set! extract-labels strict-extract-labels)
)

(define 
  (update-insts! insts labels machine)
  (let 
    ( ;
     (pc (get-register machine 'pc))
     (flag (get-register machine 'flag))
     (stack (machine 'stack))
     (ops (machine 'operations))
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
