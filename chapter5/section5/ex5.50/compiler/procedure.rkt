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
    ; list
    (list 'car car)
    (list 'cdr cdr)
    (list 'cons cons)
    (list 'pair? pair?)
    (list 'list list)
    ; car/cdr
    (list 'caar caar)
    (list 'cadr cadr)
    (list 'cdar cdar)
    (list 'cddr cddr)
    (list 'caaar caaar)
    (list 'caadr caadr)
    (list 'cadar cadar)
    (list 'caddr caddr)
    (list 'cdaar cdaar)
    (list 'cdadr cdadr)
    (list 'cddar cddar)
    (list 'cdddr cdddr)
    (list 'caaaar caaaar)
    (list 'caaadr caaadr)
    (list 'caadar caadar)
    (list 'caaddr caaddr)
    (list 'cadaar cadaar)
    (list 'cadadr cadadr)
    (list 'caddar caddar)
    (list 'cadddr cadddr)
    (list 'cdaaar cdaaar)
    (list 'cdaadr cdaadr)
    (list 'cdadar cdadar)
    (list 'cdaddr cdaddr)
    (list 'cddaar cddaar)
    (list 'cddadr cddadr)
    (list 'cdddar cdddar)
    (list 'cddddr cddddr)
    ; mutable pair
    (list 'set-car! set-car!)
    (list 'set-cdr! set-cdr!)

    ; types
    (list 'null? null?)
    (list 'number? number?)
    (list 'string? string?)
    (list 'symbol? symbol?)

    ; operators
    (list '+ +)
    (list '- -)
    (list '* *)
    (list '/ /)
    (list '= =)
    (list '< <)
    (list '> >)
    (list 'eq? eq?)
    (list 'not not)

    ; i/o
    (list 'display display)
    (list 'displayln displayln)
    (list 'println println)

    ; utils
    (list 'length length)

    ; others
    (list 'apply-in-underlying-scheme apply)
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