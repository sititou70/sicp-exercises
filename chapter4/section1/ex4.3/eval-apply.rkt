#lang racket
(provide (all-defined-out))

(require sicp)
(require "expression.rkt")
(require "internal-data-structure.rkt")
(require "primitive-procedures.rkt")

; see: http://community.schemewiki.org/?sicp-ex-2.73
(define *procedures* (make-hash))
(define (put key value) (hash-set! *procedures* key value))
(define (get key) (hash-ref *procedures* key #f))

(define 
  (eval exp env)
  (let 
    ( ;
     (proc (if (pair? exp) (get (car exp)) #f))
    )

    (cond 
      (proc (proc exp env))
      ((self-evaluating? exp) exp)
      ((variable? exp) (lookup-variable-value exp env))
      ((application? exp)
       (apply (eval (operator exp) env) 
              (list-of-values (operands exp) env)
       )
      )
      (else
       (error "Unknown expression type: EVAL" exp)
      )
    )
  )
)
(put 'quote (lambda (exp env) (text-of-quotation exp)))
(put 
  'set!
  (lambda (exp env) 
    (set-variable-value! 
      (assignment-variable exp)
      (eval (assignment-value exp) env)
      env
    )
    'ok
  )
)
(put 
  'define
  (lambda (exp env) 
    (define-variable! 
      (definition-variable exp)
      (eval (definition-value exp) env)
      env
    )
    'ok
  )
)
(put 
  'if
  (lambda (exp env) 
    (if (true? (eval (if-predicate exp) env)) 
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)
    )
  )
)
(put 
  'lambda
  (lambda (exp env) 
    (make-procedure 
      (lambda-parameters exp)
      (lambda-body exp)
      env
    )
  )
)
(put 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
(put 'cond (lambda (exp env) (eval (cond->if exp) env)))

(define 
  (apply procedure arguments)
  (let 
    ( ;
     (proc (if (pair? procedure) (get (car procedure)) #f))
    )

    (cond 
      (proc (proc procedure arguments))
      (else
       (error 
         "Unknown procedure type: APPLY"
         procedure
       )
      )
    )
  )
)
(put 
  'primitive
  (lambda (procedure arguments) 
    (apply-primitive-procedure procedure arguments)
  )
)
(put 
  'procedure
  (lambda (procedure arguments) 
    (eval-sequence 
      (procedure-body procedure)
      (extend-environment 
        (procedure-parameters procedure)
        arguments
        (procedure-environment procedure)
      )
    )
  )
)

; utils
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
