#lang racket
(provide (all-defined-out))

(require sicp)
(require "data-directed-utils.scm")

; assert
(define 
  (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!)
)

; and
(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))

; or
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))

; not
(define (negated-query exps) (car exps))

; lisp-value
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

; rule
(define 
  (rule? statement)
  (tagged-list? statement 'rule)
)
(define (conclusion rule) (cadr rule))
(define 
  (rule-body rule)
  (if (null? (cddr rule)) '(always-true) (caddr rule))
)
