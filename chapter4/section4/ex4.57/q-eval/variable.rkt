#lang racket
(provide (all-defined-out))

(require sicp)
(require "./data-directed-utils.rkt")

(define (var? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))

; generate new variables
(define rule-counter 0)
(define 
  (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter
)
(define 
  (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var)))
)
