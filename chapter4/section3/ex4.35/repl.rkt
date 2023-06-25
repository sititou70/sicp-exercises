#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "m-eval/eval-apply.rkt")
(require "m-eval/global-environment.rkt")

(define 
  (driver-loop)

  (define input-prompt "[Amb-Eval] > ")

  (define 
    (internal-loop try-again)

    (display input-prompt)
    (let 
      ( ;
       (input (read))
      )

      (if (eq? input 'try-again) 
        (try-again)
        (begin 
          (displayln ";;; Starting a new problem")
          (ambeval 
            input
            the-global-environment
            (lambda (val next-alternative) 
              (displayln val)
              (internal-loop next-alternative)
            )
            (lambda () 
              (display 
                ";;; There are no more values of "
              )
              (displayln input)
              (driver-loop)
            )
          )
        )
      )
    )
  )

  (internal-loop 
    (lambda () 
      (displayln ";;; There is no current problem")
      (driver-loop)
    )
  )
)

(driver-loop)
