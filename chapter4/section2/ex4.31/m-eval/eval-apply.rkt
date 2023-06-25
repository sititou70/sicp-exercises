#lang racket
(provide (all-defined-out))

(require sicp)
(require "tag.rkt")
(require "expression.rkt")
(require "procedure.rkt")
(require "environment.rkt")

; eval
(define 
  (eval exp env)
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
    ((cond? exp) (eval (cond->if exp) env))
    ((application? exp)
     (apply 
       (actual-value (operator exp) env)
       (operands exp)
       env
     )
    )
    (else
     (error "Unknown expression type: EVAL" exp)
    )
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
  (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env)) 
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)
  )
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

; apply
(define 
  (apply procedure arguments env)
  (cond 
    ((primitive-procedure? procedure)
     (apply-primitive-procedure 
       procedure
       (list-of-arg-values arguments env)
     )
    )
    ((compound-procedure? procedure)
     (eval-sequence 
       (procedure-body procedure)
       (extend-environment 
         (parameter-symbols (procedure-parameters procedure))
         (list-of-proc-args arguments (procedure-parameters procedure) env)
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

(define 
  (list-of-arg-values exps env)
  (if (no-operands? exps) 
    '()
    (cons 
      (actual-value 
        (first-operand exps)
        env
      )
      (list-of-arg-values 
        (rest-operands exps)
        env
      )
    )
  )
)

(define 
  (parameter-symbols params)
  (if (no-parameters? params) 
    '()
    (cons 
      (parameter-symbol (first-parameter params))
      (parameter-symbols 
        (rest-parameters params)
      )
    )
  )
)

(define 
  (list-of-proc-args args params env)
  (if (no-operands? args) 
    '()
    (cons 
      (let 
        ( ;
         (arg (first-operand args))
         (param (first-parameter params))
        )

        (cond 
          ((normal-parameter? param) (actual-value arg env))
          ((lazy-parameter? param) (delay-it arg env))
          ((lazy-memo-parameter? param) (delay-memo-it arg env))
        )
      )
      (list-of-proc-args 
        (rest-operands args)
        (rest-parameters params)
        env
      )
    )
  )
)

; thunk
(define 
  (delay-it exp env)
  (list 'thunk exp env)
)
(define 
  (delay-memo-it exp env)
  (list 'thunk-memo exp env)
)
(define 
  (thunk? obj)
  (tagged-list? obj 'thunk)
)
(define 
  (thunk-memo? obj)
  (tagged-list? obj 'thunk-memo)
)
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define 
  (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk)
)
(define 
  (thunk-value evaluated-thunk)
  (cadr evaluated-thunk)
)

(define 
  (force-it obj)
  (cond 
    ((thunk? obj)
     (let 
       ( ;
        (result 
          (actual-value 
            (thunk-exp obj)
            (thunk-env obj)
          )
        )
       )

       result
     )
    )
    ((thunk-memo? obj)
     (let 
       ( ;
        (result 
          (actual-value 
            (thunk-exp obj)
            (thunk-env obj)
          )
        )
       )

       (set-car! obj 'evaluated-thunk)
       (set-car! 
         (cdr obj)
         result
       )
       (set-cdr! 
         (cdr obj)
         '()
       )
       result
     )
    )
    ((evaluated-thunk? obj) (thunk-value obj))
    (else obj)
  )
)

; utils
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define 
  (actual-value exp env)
  (force-it (eval exp env))
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
