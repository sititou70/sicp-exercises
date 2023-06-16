#lang racket
(provide (all-defined-out))

(require sicp)
(require "./data-directed-utils.scm")

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

; transform query -> internal variable representation
(define 
  (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp)
)
(define 
  (map-over-symbols proc exp)
  (cond 
    ((pair? exp)
     (cons (map-over-symbols proc (car exp)) 
           (map-over-symbols proc (cdr exp))
     )
    )
    ((symbol? exp) (proc exp))
    (else exp)
  )
)
(define 
  (expand-question-mark symbol)
  (let 
    ( ;
     (chars (symbol->string symbol))
    )

    (if (string=? (substring chars 0 1) "?") 
      (list 
        '?
        (string->symbol 
          (substring chars 1 (string-length chars))
        )
      )
      symbol
    )
  )
)

; transform internal variable representation -> human-readable symbol
(define 
  (contract-question-mark variable)
  (string->symbol 
    (string-append 
      "?"
      (if (number? (cadr variable)) 
        (string-append 
          (symbol->string (caddr variable))
          "-"
          (number->string (cadr variable))
        )
        (symbol->string (cadr variable))
      )
    )
  )
)
