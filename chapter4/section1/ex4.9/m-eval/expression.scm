#lang racket
(provide (all-defined-out))

(require sicp)
(require "tag.scm")

(define 
  (self-evaluating? exp)
  (cond 
    ((number? exp) true)
    ((string? exp) true)
    (else false)
  )
)

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))
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
  (make-definition variable value)
  (list 'define variable value)
)

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
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

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define 
  (make-lambda parameters body)
  (cons 'lambda (cons parameters body))
)

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define 
  (sequence->exp seq)
  (cond 
    ((null? seq) seq)
    ((last-exp? seq) (first-exp seq))
    (else (make-begin seq))
  )
)
(define (make-begin seq) (cons 'begin seq))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define 
  (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else)
)
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
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
            "ELSE clause isn't last: COND->IF "
            clauses
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

(define (while? exp) (tagged-list? exp 'while))
(define (while-condition exp) (cadr exp))
(define (while-body exp) (cddr exp))
(define 
  (while->lambda exp)
  (list 
    (make-lambda 
      '()
      (list 
        (make-definition 
          'while-iter
          (make-lambda 
            '()
            (append 
              (while-body exp)
              (list 
                (make-if 
                  (while-condition exp)
                  '(while-iter)
                  'false
                )
              )
            )
          )
        )
        '(while-iter)
      )
    )
  )
)

(define (for? exp) (tagged-list? exp 'for))
(define (for-initial-bind exp) (cadr exp))
(define (for-condition exp) (caddr exp))
(define (for-afterthought exp) (cadddr exp))
(define (for-body exp) (cddddr exp))
(define 
  (for->lambda exp)
  (let 
    ( ;
     (initial-bind (for-initial-bind exp))
     (condition (for-condition exp))
     (afterthought (for-afterthought exp))
     (body (for-body exp))
    )

    (list 
      (make-lambda 
        '()
        (list 
          (make-definition (car initial-bind) (cadr initial-bind))
          (make-definition 
            'for-iter
            (make-lambda 
              '()
              (list 
                (make-if 
                  condition
                  (sequence->exp 
                    (append body (list afterthought) '((for-iter)))
                  )
                  'false
                )
              )
            )
          )
          '(for-iter)
        )
      )
    )
  )
)

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
