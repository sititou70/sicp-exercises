#lang racket
(provide (all-defined-out))

(require sicp)

(define 
  (install-tag repl)

  (repl 
    '( ;
      (define 
        (type exp)
        (if (pair? exp) 
          (car exp)
          (error "Unknown expression TYPE" exp)
        )
      )
      (define 
        (contents exp)
        (if (pair? exp) 
          (cdr exp)
          (error "Unknown expression CONTENTS" exp)
        )
      )

      (define 
        (tagged-list? exp tag)
        (if (pair? exp) 
          (eq? (car exp) tag)
          false
        )
      )
     )
  )
)
