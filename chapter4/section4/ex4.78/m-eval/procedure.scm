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
  (list 
    ; list
    (list 'cons cons)
    (list 'car car)
    (list 'cdr cdr)
    (list 'cadr cadr)
    (list 'cddr cddr)
    (list 'caddr caddr)
    (list 'pair? pair?)
    (list 'list list)

    ; list utils
    (list 'append append)
    (list 'member member)
    (list 'memq memq)
    (list 'assoc assoc)

    ; symbol
    (list 'symbol? symbol?)
    (list 'eq? eq?)
    (list 'equal? equal?)
    (list 'null? null?)
    (list 'number? number?)
    (list 'symbol->string symbol->string)
    (list 'string->symbol string->symbol)

    ; string
    (list 'string=? string=?)
    (list 'substring substring)
    (list 'string-length string-length)
    (list 'string-append string-append)

    ; logical operator
    (list '= =)
    (list '< <)
    (list '<= <=)
    (list '> >)
    (list '>= >=)
    (list 'not not)

    ; arithmetic operator
    (list '+ +)
    (list '- -)
    (list '* *)
    (list '/ /)
    (list 'remainder remainder)

    ; math
    (list 'abs abs)

    ; eval-apply
    (list 'scheme-eval eval)
    (list 'scheme-apply apply)
    (list 'scheme-make-base-namespace make-base-namespace)

    ; other
    (list 'sleep sleep)
    (list 'display display)
    (list 'displayln displayln)
    (list 'error error)
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
