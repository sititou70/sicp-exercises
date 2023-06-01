#lang racket
(provide (all-defined-out))

(require sicp)
(require "tag.scm")

; primitive-procedure
(define 
  (primitive-procedure? proc)
  (tagged-list? proc 'primitive)
)
(define (primitive-implementation proc) (cadr proc))

(define 
  primitive-procedures
  (map 
    (lambda (bind) (cons (car bind) (list 'primitive (cdr bind))))
    (list 
      (cons 'car car)
      (cons 'cdr cdr)
      (cons 'cons cons)
      (cons 'null? null?)
    )
  )
)

(define apply-in-underlying-scheme apply)
(define 
  (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme 
    (primitive-implementation proc)
    args
  )
)

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
