#lang racket
(provide (all-defined-out))

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "eval-apply.scm")
(require "environment.scm")
(require "procedure.scm")

; global environment
(define 
  (setup-environment)
  (let 
    ( ;
     (initial-env 
       (extend-environment 
         (primitive-procedure-names)
         (primitive-procedure-objects)
         the-empty-environment
       )
     )
    )

    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env
  )
)
(define the-global-environment (setup-environment))

; driver loop
(define input-prompt "[M-Eval]")
(define 
  (driver-loop)

  (prompt-for-input input-prompt)
  (let 
    ( ;
     (input (read))
    )

    (let 
      ( ;
       (output (eval input the-global-environment))
      )

      (user-print output)
      (newline)
    )
  )
  (driver-loop)
)

(define 
  (prompt-for-input string)
  (display string)
  (display " > ")
)

(define 
  (user-print object)
  (if (compound-procedure? object) 
    (display 
      (list 'compound-procedure 
            (procedure-parameters object)
            (procedure-body object)
            '<procedure-env>
      )
    )
    (display object)
  )
)
