#lang racket

(begin 
  ; utils
  (define 
    (map proc items)
    (if (null? items) 
      '()
      (cons (proc (car items)) (map proc (cdr items)))
    )
  )

  ; ####################
  ; # tag.rkt
  ; ####################
  (define 
    (tagged-list? exp tag)
    (if (pair? exp) 
      (eq? (car exp) tag)
      false
    )
  )

  ; ####################
  ; # expression.rkt
  ; ####################
  (define 
    (self-evaluating? exp)
    (cond 
      ((number? exp) true)
      (else false)
    )
  )

  (define 
    (variable? exp)
    (symbol? exp)
  )

  (define 
    (quoted? exp)
    (tagged-list? exp 'quote)
  )
  (define 
    (text-of-quotation exp)
    (cadr exp)
  )

  (define 
    (assignment? exp)
    (tagged-list? exp 'set!)
  )
  (define 
    (assignment-variable exp)
    (cadr exp)
  )
  (define 
    (assignment-value exp)
    (caddr exp)
  )

  (define 
    (definition? exp)
    (tagged-list? exp 'define)
  )
  (define 
    (definition-variable exp)
    (if (symbol? (cadr exp)) 
      (cadr exp)
      (caadr exp)
    )
  )
  (define 
    (definition-value exp)
    (if (symbol? (cadr exp)) 
      (caddr exp)
      (make-lambda 
        (cdadr exp) ; 仮引数
        (cddr exp) ; 本体
      )
    )
  )

  (define 
    (if? exp)
    (tagged-list? exp 'if)
  )
  (define 
    (if-predicate exp)
    (cadr exp)
  )
  (define 
    (if-consequent exp)
    (caddr exp)
  )
  (define 
    (if-alternative exp)
    (if (not (null? (cdddr exp))) 
      (cadddr exp)
      'false
    )
  )
  (define 
    (make-if predicate consequent alternative)
    (list 'if predicate consequent alternative)
  )

  (define 
    (lambda? exp)
    (tagged-list? exp 'lambda)
  )
  (define 
    (lambda-parameters exp)
    (cadr exp)
  )
  (define 
    (lambda-body exp)
    (cddr exp)
  )
  (define 
    (make-lambda parameters body)
    (cons 'lambda (cons parameters body))
  )

  (define 
    (begin? exp)
    (tagged-list? exp 'begin)
  )
  (define 
    (begin-actions exp)
    (cdr exp)
  )
  (define 
    (last-exp? seq)
    (null? (cdr seq))
  )
  (define 
    (first-exp seq)
    (car seq)
  )
  (define 
    (rest-exps seq)
    (cdr seq)
  )
  (define 
    (sequence->exp seq)
    (cond 
      ((null? seq) seq)
      ((last-exp? seq) (first-exp seq))
      (else (make-begin seq))
    )
  )
  (define 
    (make-begin seq)
    (cons 'begin seq)
  )

  (define 
    (cond? exp)
    (tagged-list? exp 'cond)
  )
  (define 
    (cond-clauses exp)
    (cdr exp)
  )
  (define 
    (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else)
  )
  (define 
    (cond-predicate clause)
    (car clause)
  )
  (define 
    (cond-actions clause)
    (cdr clause)
  )
  (define 
    (cond->if exp)
    (expand-clauses (cond-clauses exp))
  )
  (define 
    (expand-clauses clauses)
    (if (null? clauses) 
      'false ; else 節はない
      (let 
        ( ;
         (first (car clauses))
         (rest (cdr clauses))
        )

        (if (cond-else-clause? first) 
          (if (null? rest) 
            (sequence->exp (cond-actions first))
            (error 
              'ELSE_clause_isn_t_last_COND_to_IF
            )
          )
          (make-if 
            (cond-predicate first)
            (sequence->exp (cond-actions first))
            (expand-clauses rest)
          )
        )
      )
    )
  )

  (define 
    (application? exp)
    (pair? exp)
  )
  (define 
    (operator exp)
    (car exp)
  )
  (define 
    (operands exp)
    (cdr exp)
  )
  (define 
    (no-operands? ops)
    (null? ops)
  )
  (define 
    (first-operand ops)
    (car ops)
  )
  (define 
    (rest-operands ops)
    (cdr ops)
  )

  ; ####################
  ; # procedure.rkt
  ; ####################
  ;; primitive-procedure
  (define 
    (primitive-procedure? proc)
    (tagged-list? proc 'primitive)
  )
  (define 
    (primitive-implementation proc)
    (cadr proc)
  )

  (define 
    primitive-procedures
    (list 
      ; list
      (list 'car car)
      (list 'cdr cdr)
      (list 'cons cons)
      (list 'pair? pair?)
      (list 'list list)

      ; types
      (list 'null? null?)
      (list 'number? number?)
      (list 'symbol? symbol?)

      ; operators
      (list '+ +)
      (list '- -)
      (list '< <)
      (list '= eq?)
      (list 'eq? eq?)
      (list 'not not)

      ; i/o
      (list 'displayln displayln)
    )
  )
  (define 
    (primitive-procedure-names)
    (map car primitive-procedures)
  )
  (define 
    (primitive-procedure-objects)
    (map 
      (lambda (proc) 
        (list 
          'primitive
          (cadadr proc)
        )
      )
      primitive-procedures
    )
  )

  (define 
    (apply-primitive-procedure proc args)
    (primitive-base-apply 
      (primitive-implementation proc)
      args
    )
  )

  ;; compound-procedure
  (define 
    (make-procedure parameters body env)
    (list 'procedure parameters body env)
  )
  (define 
    (compound-procedure? p)
    (tagged-list? p 'procedure)
  )
  (define 
    (procedure-parameters p)
    (cadr p)
  )
  (define 
    (procedure-body p)
    (caddr p)
  )
  (define 
    (procedure-environment p)
    (cadddr p)
  )

  ; ####################
  ; # environment.rkt
  ; ####################
  ;; environment
  (define 
    (enclosing-environment env)
    (cdr env)
  )
  (define 
    (first-frame env)
    (car env)
  )
  (define 
    the-empty-environment
    '()
  )

  (define 
    (make-frame variables values)
    (cons variables values)
  )
  (define 
    (frame-variables frame)
    (car frame)
  )
  (define 
    (frame-values frame)
    (cdr frame)
  )
  (define 
    (add-binding-to-frame! var val frame)
    (set-car! frame (cons var (car frame)))
    (set-cdr! frame (cons val (cdr frame)))
  )

  ;; utils
  (define 
    (extend-environment vars vals base-env)
    (if (= (length vars) (length vals)) 
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals)) 
        (error 'Too_many_arguments_supplied)
        (error 'Too_few_arguments_supplied)
      )
    )
  )

  (define 
    (lookup-variable-value var env)
    (define 
      (env-loop env)
      (define 
        (scan vars vals)
        (cond 
          ((null? vars)
           (env-loop (enclosing-environment env))
          )
          ((eq? var (car vars)) (car vals))
          (else (scan (cdr vars) (cdr vals)))
        )
      )
      (if (eq? env the-empty-environment) 
        (error 'Unbound_variable)
        (let 
          ((frame (first-frame env)))
          (scan 
            (frame-variables frame)
            (frame-values frame)
          )
        )
      )
    )
    (env-loop env)
  )

  (define 
    (set-variable-value! var val env)
    (define 
      (env-loop env)
      (define 
        (scan vars vals)
        (cond 
          ((null? vars)
           (env-loop (enclosing-environment env))
          )
          ((eq? var (car vars)) (set-car! vals val))
          (else (scan (cdr vars) (cdr vals)))
        )
      )
      (if (eq? env the-empty-environment) 
        (error 
          'Unbound_variable_SET
        )
        (let 
          ( ;
           (frame (first-frame env))
          )

          (scan 
            (frame-variables frame)
            (frame-values frame)
          )
        )
      )
    )
    (env-loop env)
  )

  (define 
    (define-variable! var val env)
    (let 
      ( ;
       (frame (first-frame env))
      )

      (define 
        (scan vars vals)
        (cond 
          ((null? vars)
           (add-binding-to-frame! var val frame)
          )
          ((eq? var (car vars)) (set-car! vals val))
          (else (scan (cdr vars) (cdr vals)))
        )
      )

      (scan (frame-variables frame) (frame-values frame))
    )
  )

  ; ####################
  ; # global-environment.rkt
  ; ####################
  (define 
    (setup-environment)
    (let 
      ( ;
       (initial-env 
         (extend-environment 
           (primitive-procedure-names)
           (primitive-procedure-objects)
           the-empty-environment
         )
       )
      )

      (define-variable! 'true true initial-env)
      (define-variable! 'false false initial-env)
      initial-env
    )
  )
  (define the-global-environment (setup-environment))

  ; ####################
  ; # eval-apply.rkt
  ; ####################
  ;; eval
  (define 
    (eval exp env)
    (cond 
      ((self-evaluating? exp) exp)
      ((variable? exp) (lookup-variable-value exp env))
      ((quoted? exp)
       (text-of-quotation 
         exp
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
      ((application? exp)
       (apply 
         (eval (operator exp) env)
         (list-of-values (operands exp) env)
       )
      )
      (else
       (error 'Unknown_expression_type_EVAL exp)
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
    (if (true? (eval (if-predicate exp) env)) 
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)
    )
  )

  (define 
    (list-of-values exps env)
    (if (no-operands? exps) 
      '()
      (cons 
        (eval (first-operand exps) env)
        (list-of-values (rest-operands exps) env)
      )
    )
  )

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
         'Unknown_procedure_type_APPLY
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

  (eval (read-from-file) the-global-environment)
)