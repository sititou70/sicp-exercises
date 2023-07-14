#lang racket
(provide (all-defined-out))

(require sicp)
(require "expressions.rkt")
(require "basic-machine-model.rkt")
(require "data-structures/register.rkt")
(require "data-structures/stack.rkt")

(define 
  (make-execution-procedure inst labels machine pc flag stack ops)
  (cond 
    ((eq? (car inst) 'assign)
     (make-assign inst machine labels ops pc)
    )
    ((eq? (car inst) 'test)
     (make-test inst machine labels ops flag pc)
    )
    ((eq? (car inst) 'branch)
     (make-branch inst machine labels flag pc)
    )
    ((eq? (car inst) 'goto)
     (make-goto inst machine labels pc)
    )
    ((eq? (car inst) 'save)
     (make-save inst machine stack pc)
    )
    ((eq? (car inst) 'restore)
     (make-restore inst machine stack pc)
    )
    ((eq? (car inst) 'perform)
     (make-perform inst machine labels ops pc)
    )
    (else
     (error 
       "Unknown instruction type: ASSEMBLE"
       inst
     )
    )
  )
)

(define 
  (make-assign inst machine labels operations pc)
  (let 
    ( ;
     (target 
       (get-register machine (assign-reg-name inst))
     )
     (value-exp (assign-value-exp inst))
    )

    (let 
      ( ;
       (value-proc 
         (if (operation-exp? value-exp) 
           (make-operation-exp 
             value-exp
             machine
             labels
             operations
           )
           (make-primitive-exp 
             (car value-exp)
             machine
             labels
           )
         )
       )
      )

      (lambda () 
        (set-contents! target (value-proc))
        (advance-pc pc)
      )
    )
  )
)

(define 
  (make-test inst machine labels operations flag pc)
  (let 
    ( ;
     (condition (test-condition inst))
    )

    (if (operation-exp? condition) 
      (let 
        ( ;
         (condition-proc 
           (make-operation-exp 
             condition
             machine
             labels
             operations
           )
         )
        )

        (lambda () 
          (set-contents! flag (condition-proc))
          (advance-pc pc)
        )
      )
      (error "Bad TEST instruction: ASSEMBLE" inst)
    )
  )
)

(define 
  (make-branch inst machine labels flag pc)
  (let 
    ( ;
     (dest (branch-dest inst))
    )

    (if (label-exp? dest) 
      (let 
        ( ;
         (insts 
           (lookup-label 
             labels
             (label-exp-label dest)
           )
         )
        )
        (lambda () 
          (if (get-contents flag) 
            (set-contents! pc insts)
            (advance-pc pc)
          )
        )
      )
      (error "Bad BRANCH instruction: ASSEMBLE" inst)
    )
  )
)

(define 
  (make-goto inst machine labels pc)
  (let 
    ( ;
     (dest (goto-dest inst))
    )

    (cond 
      ((label-exp? dest)
       (let 
         ( ;
          (insts 
            (lookup-label 
              labels
              (label-exp-label dest)
            )
          )
         )
         (lambda () (set-contents! pc insts))
       )
      )
      ((register-exp? dest)
       (let 
         ( ;
          (reg 
            (get-register 
              machine
              (register-exp-reg dest)
            )
          )
         )
         (lambda () 
           (set-contents! pc (get-contents reg))
         )
       )
      )
      (else (error "Bad GOTO instruction: ASSEMBLE" inst))
    )
  )
)

(define 
  (make-save inst machine stack pc)
  (let 
    ( ;
     (reg 
       (get-register 
         machine
         (stack-inst-reg-name inst)
       )
     )
    )

    (lambda () 
      (push stack (get-contents reg))
      (advance-pc pc)
    )
  )
)
(define 
  (make-restore inst machine stack pc)
  (let 
    ( ;
     (reg 
       (get-register 
         machine
         (stack-inst-reg-name inst)
       )
     )
    )

    (lambda () 
      (set-contents! reg (pop stack))
      (advance-pc pc)
    )
  )
)

(define 
  (make-perform inst machine labels operations pc)
  (let 
    ( ;
     (action (perform-action inst))
    )

    (if (operation-exp? action) 
      (let 
        ( ;
         (action-proc 
           (make-operation-exp 
             action
             machine
             labels
             operations
           )
         )
        )

        (lambda () 
          (action-proc)
          (advance-pc pc)
        )
      )
      (error "Bad PERFORM instruction: ASSEMBLE " inst)
    )
  )
)

(define 
  (make-primitive-exp exp machine labels)
  (cond 
    ((constant-exp? exp)
     (let 
       ( ;
        (c (constant-exp-value exp))
       )

       (lambda () c)
     )
    )
    ((label-exp? exp)
     (let 
       ( ;
        (insts 
          (lookup-label 
            labels
            (label-exp-label exp)
          )
        )
       )

       (lambda () insts)
     )
    )
    ((register-exp? exp)
     (let 
       ( ;
        (r (get-register machine (register-exp-reg exp)))
       )

       (lambda () (get-contents r))
     )
    )
    (else (error "Unknown expression type: ASSEMBLE" exp))
  )
)

(define 
  (make-operation-exp exp machine labels operations)
  (let 
    ( ;
     (op 
       (lookup-prim 
         (operation-exp-op exp)
         operations
       )
     )
     (aprocs 
       (map 
         (lambda (e) 
           (make-primitive-exp e machine labels)
         )
         (operation-exp-operands exp)
       )
     )
    )
    (lambda () 
      (apply 
        op
        (map (lambda (p) (p)) aprocs)
      )
    )
  )
)

; utils
(define 
  (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc)))
)

(define 
  (lookup-label labels label-name)
  (let 
    ( ;
     (val (assoc label-name labels))
    )

    (if val 
      (cdr val)
      (error 
        "Undefined label: ASSEMBLE"
        label-name
      )
    )
  )
)

(define 
  (lookup-prim symbol operations)
  (let 
    ( ;
     (val (assoc symbol operations))
    )

    (if val 
      (cadr val)
      (error 
        "Unknown operation: ASSEMBLE"
        symbol
      )
    )
  )
)