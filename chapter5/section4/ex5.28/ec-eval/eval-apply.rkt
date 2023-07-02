#lang racket
(provide (all-defined-out))

(require sicp)
(require "expression.rkt")
(require "environment.rkt")
(require "procedure.rkt")

(define 
  eval-insts
  '( ;
    eval-dispatch
    (test (op self-evaluating?) (reg exp))
    (branch (label ev-self-eval))
    (test (op variable?) (reg exp))
    (branch (label ev-variable))
    (test (op quoted?) (reg exp))
    (branch (label ev-quoted))
    (test (op assignment?) (reg exp))
    (branch (label ev-assignment))
    (test (op definition?) (reg exp))
    (branch (label ev-definition))
    (test (op if?) (reg exp))
    (branch (label ev-if))
    (test (op lambda?) (reg exp))
    (branch (label ev-lambda))
    (test (op begin?) (reg exp))
    (branch (label ev-begin))
    (test (op cond?) (reg exp))
    (branch (label ev-cond))
    (test (op let?) (reg exp))
    (branch (label ev-let))
    (test (op application?) (reg exp))
    (branch (label ev-application))
    (goto (label unknown-expression-type))

    ; self-eval
    ev-self-eval
    (assign val (reg exp))
    (goto (reg continue))

    ; variable
    ev-variable
    (assign val (op lookup-variable-value) (reg exp) (reg env))
    (goto (reg continue))

    ;quoted
    ev-quoted
    (assign val (op text-of-quotation) (reg exp))
    (goto (reg continue))

    ; assignment
    ev-assignment
    (assign unev (op assignment-variable) (reg exp))
    (save unev) ; 後で使うために変数を保存
    (assign exp (op assignment-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label ev-assignment-1))
    (goto (label eval-dispatch)) ; 代⼊値を評価

    ev-assignment-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform 
      (op set-variable-value!)
      (reg unev)
      (reg val)
      (reg env)
    )
    (assign val (const ok))
    (goto (reg continue))

    ; definition
    ev-definition
    (assign unev (op definition-variable) (reg exp))
    (save unev) ; 後で使うために変数を保存
    (assign exp (op definition-value) (reg exp))
    (save env)
    (save continue)
    (assign continue (label ev-definition-1))
    (goto (label eval-dispatch)) ; 定義値を評価

    ev-definition-1
    (restore continue)
    (restore env)
    (restore unev)
    (perform 
      (op define-variable!)
      (reg unev)
      (reg val)
      (reg env)
    )
    (assign val (const ok))
    (goto (reg continue))

    ; if
    ev-if
    (save exp) ; 後で使うために式を保存
    (save env)
    (save continue)
    (assign continue (label ev-if-decide))
    (assign exp (op if-predicate) (reg exp))
    (goto (label eval-dispatch)) ; 述語を評価

    ev-if-decide
    (restore continue)
    (restore env)
    (restore exp)
    (test (op true?) (reg val))
    (branch (label ev-if-consequent))

    ev-if-alternative
    (assign exp (op if-alternative) (reg exp))
    (goto (label eval-dispatch))

    ev-if-consequent
    (assign exp (op if-consequent) (reg exp))
    (goto (label eval-dispatch))

    ; lambda
    ev-lambda
    (assign unev (op lambda-parameters) (reg exp))
    (assign exp (op lambda-body) (reg exp))
    (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
    (goto (reg continue))

    ; begin
    ev-begin
    (assign unev (op begin-actions) (reg exp))
    (save continue)
    (goto (label ev-sequence))

    ; cond
    ev-cond
    (assign exp (op cond->if) (reg exp))
    (goto (label eval-dispatch))

    ; let
    ev-let
    (assign exp (op let->combination) (reg exp))
    (goto (label eval-dispatch))

    ; application
    ev-application
    (save continue)
    (save env)
    (assign unev (op operands) (reg exp))
    (save unev)
    (assign exp (op operator) (reg exp))
    (assign continue (label ev-appl-did-operator))
    (goto (label eval-dispatch))

    ev-appl-did-operator
    (restore unev) ; the operands
    (restore env)
    (assign argl (op empty-arglist))
    (assign proc (reg val)) ; the operator
    (test (op no-operands?) (reg unev))
    (branch (label apply-dispatch))
    (save proc)

    ev-appl-operand-loop
    (save argl)
    (assign exp (op first-operand) (reg unev))
    (test (op last-operand?) (reg unev))
    (branch (label ev-appl-last-arg))
    (save env)
    (save unev)
    (assign continue (label ev-appl-accumulate-arg))
    (goto (label eval-dispatch))

    ev-appl-accumulate-arg
    (restore unev)
    (restore env)
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (assign unev (op rest-operands) (reg unev))
    (goto (label ev-appl-operand-loop))

    ev-appl-last-arg
    (assign continue (label ev-appl-accum-last-arg))
    (goto (label eval-dispatch))

    ev-appl-accum-last-arg
    (restore argl)
    (assign argl (op adjoin-arg) (reg val) (reg argl))
    (restore proc)
    (goto (label apply-dispatch))

    ; eval utils
    ev-sequence
    (test (op no-more-exps?) (reg unev))
    (branch (label ev-sequence-end))
    (assign exp (op first-exp) (reg unev))
    (save unev)
    (save env)
    (assign continue (label ev-sequence-continue))
    (goto (label eval-dispatch))

    ev-sequence-continue
    (restore env)
    (restore unev)
    (assign unev (op rest-exps) (reg unev))
    (goto (label ev-sequence))

    ev-sequence-end
    (restore continue)
    (goto (reg continue))
   )
)

(define 
  apply-insts
  '( ;
    apply-dispatch
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-apply))
    (test (op compound-procedure?) (reg proc))
    (branch (label compound-apply))
    (goto (label unknown-procedure-type))

    primitive-apply
    (assign 
      val
      (op apply-primitive-procedure)
      (reg proc)
      (reg argl)
    )
    (restore continue)
    (goto (reg continue))

    compound-apply
    (assign unev (op procedure-parameters) (reg proc))
    (assign env (op procedure-environment) (reg proc))
    (assign 
      env
      (op extend-environment)
      (reg unev)
      (reg argl)
      (reg env)
    )
    (assign unev (op procedure-body) (reg proc))
    (goto (label ev-sequence))
   )
)

; utils
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (empty-arglist) '())
(define (adjoin-arg arg arglist) (append arglist (list arg)))
(define (last-operand? ops) (null? (cdr ops)))
(define (no-more-exps? seq) (null? seq))

; operations
(define 
  eval-operations
  (list 
    ; dispatch
    (list 'self-evaluating? self-evaluating?)
    (list 'variable? variable?)
    (list 'quoted? quoted?)
    (list 'assignment? assignment?)
    (list 'definition? definition?)
    (list 'if? if?)
    (list 'lambda? lambda?)
    (list 'begin? begin?)
    (list 'cond? cond?)
    (list 'let? let?)
    (list 'application? application?)

    ; self-eval

    ; variable
    (list 'lookup-variable-value lookup-variable-value)

    ; 'quoted
    (list 'text-of-quotation text-of-quotation)

    ; assignment
    (list 'assignment-variable assignment-variable)
    (list 'assignment-value assignment-value)
    (list 'set-variable-value! set-variable-value!)

    ; definition
    (list 'definition-variable definition-variable)
    (list 'definition-value definition-value)
    (list 'define-variable! define-variable!)

    ; if
    (list 'if-predicate if-predicate)
    (list 'true? true?)
    (list 'if-alternative if-alternative)
    (list 'if-consequent if-consequent)

    ; lambda
    (list 'lambda-parameters lambda-parameters)
    (list 'lambda-body lambda-body)
    (list 'make-procedure make-procedure)

    ; begin
    (list 'begin-actions begin-actions)

    ; cond
    (list 'cond->if cond->if)

    ; let
    (list 'let->combination let->combination)

    ; application
    (list 'operands operands)
    (list 'operator operator)
    (list 'empty-arglist empty-arglist)
    (list 'no-operands? no-operands?)
    (list 'first-operand first-operand)
    (list 'last-operand? last-operand?)
    (list 'adjoin-arg adjoin-arg)
    (list 'rest-operands rest-operands)
    (list 'adjoin-arg adjoin-arg)

    ; eval utils
    (list 'first-exp first-exp)
    (list 'last-exp? last-exp?)
    (list 'no-more-exps? no-more-exps?)
    (list 'rest-exps rest-exps)
  )
)

(define 
  apply-operations
  (list 
    ; apply-dispatch
    (list 'primitive-procedure? primitive-procedure?)
    (list 'compound-procedure? compound-procedure?)

    ; primitive-apply
    (list 'apply-primitive-procedure apply-primitive-procedure)

    ; compound-apply
    (list 'procedure-parameters procedure-parameters)
    (list 'procedure-environment procedure-environment)
    (list 'extend-environment extend-environment)
    (list 'procedure-body procedure-body)
  )
)
