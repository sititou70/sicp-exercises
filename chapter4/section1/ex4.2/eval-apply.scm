#lang racket
(provide (all-defined-out))

(require sicp)
(require "expression.scm")
(require "internal-data-structure.scm")
(require "primitive-procedures.scm")

(define 
  (eval exp env)
  (cond 
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted? exp) (text-of-quotation exp))
    ((application? exp)
     (apply (eval (operator exp) env) 
            (list-of-values (operands exp) env)
     )
    )
    ((assignment? exp) (eval-assignment exp env))
    ((definition? exp) (eval-definition exp env))
    ((if? exp) (eval-if exp env))
    ((lambda? exp)
     (make-procedure 
       (lambda-parameters exp)
       (lambda-body exp)
       env
     )
    )
    ((begin? exp)
     (eval-sequence (begin-actions exp) env)
    )
    ((cond? exp) (eval (cond->if exp) env))
    (else
     (error "Unknown expression type: EVAL" exp)
    )
  )
)

(define 
  (apply procedure arguments)
  (cond 
    ((primitive-procedure? procedure)
     (apply-primitive-procedure procedure arguments)
    )
    ((compound-procedure? procedure)
     (eval-sequence 
       (procedure-body procedure)
       (extend-environment 
         (procedure-parameters procedure)
         arguments
         (procedure-environment procedure)
       )
     )
    )
    (else
     (error 
       "Unknown procedure type: APPLY"
       procedure
     )
    )
  )
)

; utils
(define 
  (eval-if exp env)
  (if (true? (eval (if-predicate exp) env)) 
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)
  )
)

(define 
  (eval-assignment exp env)
  (set-variable-value! 
    (assignment-variable exp)
    (eval (assignment-value exp) env)
    env
  )
  'ok
)

(define 
  (eval-definition exp env)
  (define-variable! 
    (definition-variable exp)
    (eval (definition-value exp) env)
    env
  )
  'ok
)

(define 
  (list-of-values exps env)
  (if (no-operands? exps) 
    '()
    (cons (eval (first-operand exps) env) 
          (list-of-values (rest-operands exps) env)
    )
  )
)

(define 
  (eval-sequence exps env)
  (cond 
    ((last-exp? exps)
     (eval (first-exp exps) env)
    )
    (else
     (eval (first-exp exps) env)
     (eval-sequence (rest-exps exps) env)
    )
  )
)

; 基底となるLispの実装に関わらず、左の被演算子から評価する
(define 
  (list-of-values-left exps env)
  (if (no-operands? exps) 
    '()
    (let 
      ( ;
       (first (eval (first-operand exps) env))
      )

      (let 
        ( ;
         (rest (list-of-values (rest-operands exps) env))
        )

        (cons first rest)
      )
    )
  )
)
(define (use-list-of-values-left) (set! list-of-values list-of-values-left))

; 基底となるLispの実装に関わらず、右の被演算子から評価する
(define 
  (list-of-values-right exps env)
  (if (no-operands? exps) 
    '()
    (let 
      ( ;
       (rest (list-of-values (rest-operands exps) env))
      )

      (let 
        ( ;
         (first (eval (first-operand exps) env))
        )

        (cons first rest)
      )
    )
  )
)
(define (use-list-of-values-right) (set! list-of-values list-of-values-right))
