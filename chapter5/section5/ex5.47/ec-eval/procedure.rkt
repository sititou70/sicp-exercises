#lang racket
(provide (all-defined-out))

(require sicp)
(require "../common/tag.rkt")

; compound-procedure
(define 
  (make-procedure parameters body env)
  (list 'procedure parameters body env)
)
(define 
  (compound-procedure? p)
  (tagged-list? p 'procedure)
)
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
