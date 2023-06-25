#lang racket
(provide (all-defined-out))

(require sicp)
(require "tag.rkt")

(define 
  (self-evaluating? exp)
  (cond 
    ((number? exp) true)
    ((string? exp) true)
    (else false)
  )
)

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'クォート))
(define (text-of-quotation exp) (cadr exp))

(define (assignment? exp) (tagged-list? exp '代入！))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp '定義))
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

(define (if? exp) (tagged-list? exp 'もし))
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
  (list 'もし predicate consequent alternative)
)

(define (lambda? exp) (tagged-list? exp 'ラムダ))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define 
  (make-lambda parameters body)
  (cons 'ラムダ (cons parameters body))
)

(define (begin? exp) (tagged-list? exp '開始))
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

(define (cond? exp) (tagged-list? exp '条件分岐))
(define (cond-clauses exp) (cdr exp))
(define 
  (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'それ以外)
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

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
