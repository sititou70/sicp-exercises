#lang racket
(provide (all-defined-out))

(require sicp)
(require "tag.scm")
(require "expression.scm")

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
  (list 'procedure parameters (scan-out-defines body) env)
)
(define 
  (compound-procedure? p)
  (tagged-list? p 'procedure)
)
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
(define 
  (scan-out-defines body)

  (define 
    (scan-out body)
    (cond 
      ((null? body) '())
      ((tagged-list? (car body) 'define)
       (cons (cons 'set! (cdar body)) (scan-out (cdr body)))
      )
      (else (cons (car body) (scan-out (cdr body))))
    )
  )

  (define 
    (get-binds binds body)
    (cond 
      ((null? body) binds)
      ((tagged-list? (car body) 'define)
       (get-binds 
         (cons 
           (list 
             (cadar body)
             ; 単に「'*unassigned*」と書くと、メタ言語上では「*unassigned*という変数」という意味になってしまう
             (quote '*unassigned*)
           )
           binds
         )
         (cdr body)
       )
      )
      (else
       (get-binds binds (cdr body))
      )
    )
  )

  (let 
    ( ;
     (let-binds (get-binds '() body))
     (let-body (scan-out body))
    )

    (if (> (length let-binds) 0) 
      (list (make-let let-binds let-body))
      body
    )
  )
)
