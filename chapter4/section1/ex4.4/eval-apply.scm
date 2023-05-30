#lang racket
(provide (all-defined-out))

(require sicp)
(require "expression.scm")
(require "procedure.scm")
(require "environment.scm")

; eval1: andとorを特殊形式として処理する
(define 
  (eval1 exp env)
  (cond 
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted? exp) (text-of-quotation exp))
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
    ((cond? exp) (eval1 (cond->if exp) env))
    ((and? exp) (eval-and exp env))
    ((or? exp) (eval-or exp env))
    ((application? exp)
     (apply (eval1 (operator exp) env) 
            (list-of-values (operands exp) env)
     )
    )
    (else
     (error "Unknown expression type: EVAL" exp)
    )
  )
)
(define eval eval1)

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
  (eval-if exp env)
  (if (true? (eval (if-predicate exp) env)) 
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)
  )
)

(define 
  (eval-and exp env)
  (define 
    (iter exps)
    (let 
      ( ;
       (first (eval (car exps) env))
      )

      (if (null? (cdr exps)) 
        first
        (if (true? first) 
          (iter (cdr exps))
          false
        )
      )
    )
  )

  (iter (cons 'true (and-expressions exp)))
)

(define 
  (eval-or exp env)
  (define 
    (iter exps)
    (let 
      ( ;
       (first (eval (car exps) env))
      )

      (if (null? (cdr exps)) 
        first
        (if (true? first) 
          first
          (iter (cdr exps))
        )
      )
    )
  )

  (iter (cons 'false (and-expressions exp)))
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

; eval2: andとorを派生式として処理する
(define 
  (eval2 exp env)
  (cond 
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted? exp) (text-of-quotation exp))
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
    ((cond? exp) (eval2 (cond->if exp) env))
    ((and? exp) (eval2 (and->if exp) env))
    ((or? exp) (eval2 (or->if exp) env))
    ((application? exp)
     (apply (eval2 (operator exp) env) 
            (list-of-values (operands exp) env)
     )
    )
    (else
     (error "Unknown expression type: EVAL" exp)
    )
  )
)
(define (use-eval2) (set! eval eval2))

; apply
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
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

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
