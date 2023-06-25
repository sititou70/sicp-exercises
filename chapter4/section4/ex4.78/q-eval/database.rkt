#lang racket
(provide (all-defined-out))

(require sicp)

(define 
  (install-database repl)

  (repl 
    '( ;
      ; assertion
      (define THE-ASSERTIONS '())
      (define 
        (fetch-assertion)
        (amb-list THE-ASSERTIONS)
      )
      (define 
        (add-assertion! assertion)
        (set! 
          THE-ASSERTIONS
          (cons assertion THE-ASSERTIONS)
        )
      )

      ; rule
      (define THE-RULES '())
      (define 
        (fetch-rule)
        (amb-list THE-RULES)
      )
      (define 
        (add-rule! rule)
        (set! 
          THE-RULES
          (cons rule THE-RULES)
        )
      )

      ; utils
      (define 
        (add-rule-or-assertion! assertion)
        (if (rule? assertion) 
          (add-rule! assertion)
          (add-assertion! assertion)
        )
      )

      (define 
        (amb-list list)
        (require (not (null? list)))
        (amb 
          (car list)
          (amb-list (cdr list))
        )
      )
     )
  )
)
