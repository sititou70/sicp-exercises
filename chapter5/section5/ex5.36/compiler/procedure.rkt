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
  (list 
    (list 'car car)
    (list 'cdr cdr)
    (list 'cons cons)
    (list 'list list)
    (list 'null? null?)
    (list '= =)
    (list '< <)
    (list '+ +)
    (list '- -)
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

; compiled-procedure
(define 
  (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env)
)
(define 
  (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure)
)
(define 
  (compiled-procedure-entry c-proc)
  (cadr c-proc)
)
(define 
  (compiled-procedure-env c-proc)
  (caddr c-proc)
)