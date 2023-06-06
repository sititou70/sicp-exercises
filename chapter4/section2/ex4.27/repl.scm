#lang racket
(provide (all-defined-out))

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "m-eval/eval-apply.scm")
(require "m-eval/procedure.scm")
(require "m-eval/global-environment.scm")

; driver loop
(define input-prompt "[L-Eval]")
(define 
  (driver-loop)

  (prompt-for-input input-prompt)
  (let 
    ( ;
     (input (read))
    )

    (let 
      ( ;
       (output (actual-value input the-global-environment))
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

; main
(driver-loop)
