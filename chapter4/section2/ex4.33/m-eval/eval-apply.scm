#lang racket
(provide (all-defined-out))

(require sicp)
(require "tag.scm")
(require "expression.scm")
(require "procedure.scm")
(require "environment.scm")

; eval
(define 
  (eval exp env)
  (cond 
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted-list? exp) (eval (quoted-list->list exp) env))
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
         (procedure-parameters procedure)
         (list-of-delayed-args arguments env)
         (procedure-environment procedure)
       )
     )
    )
    (else
     (error 
       " Unknown procedure type: APPLY"
       procedure
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
  (thunk? obj)
  (tagged-list? obj 'thunk)
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
       ((result 
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
  (list-of-delayed-args exps env)
  (if (no-operands? exps) 
    '()
    (cons 
      (delay-it 
        (first-operand exps)
        env
      )
      (list-of-delayed-args 
        (rest-operands exps)
        env
      )
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
