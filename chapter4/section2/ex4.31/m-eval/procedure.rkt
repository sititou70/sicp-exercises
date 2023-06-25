#lang racket
(provide (all-defined-out))

(require sicp)
(require "tag.rkt")

; primitive-procedure
(define 
  (primitive-procedure? proc)
  (tagged-list? proc 'primitive)
)
(define (primitive-implementation proc) (cadr proc))

(define 
  primitive-procedures
  (list (list 'car car) 
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'displayln displayln)
        (list 'list list)
  )
)
(define 
  (primitive-procedure-names)
  (map car primitive-procedures)
)
(define 
  (primitive-procedure-objects)
  (map 
    (lambda (proc) (list 'primitive (cadr proc)))
    primitive-procedures
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

(define (normal-parameter? p) (symbol? p))
(define (lazy-parameter? p) (and (pair? p) (eq? (cadr p) 'lazy)))
(define (lazy-memo-parameter? p) (and (pair? p) (eq? (cadr p) 'lazy-memo)))

(define 
  (parameter-symbol p)
  (cond 
    ((normal-parameter? p) p)
    (else (car p))
  )
)

(define (no-parameters? ops) (null? ops))
(define (first-parameter ops) (car ops))
(define (rest-parameters ops) (cdr ops))

