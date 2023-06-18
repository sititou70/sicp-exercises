#lang racket
(provide (all-defined-out))

(require sicp)

; setup amb evaluator
(require "m-eval/eval-apply.scm")
(require "m-eval/global-environment.scm")
(define 
  (amb-repl-for-install inputs)
  (map 
    (lambda (input) 
      (ambeval 
        input
        the-global-environment
        (lambda (val next-alternative) (void))
        (lambda () (displayln "[m-eval] install error"))
      )
    )
    inputs
  )
  (void)
)

; setup query evaluator
;; install evaluator functions
(require "q-eval/tag.scm")
(install-tag amb-repl-for-install)

(require "q-eval/require.scm")
(install-require amb-repl-for-install)

(require "q-eval/and-or.scm")
(install-and-or amb-repl-for-install)

(require "q-eval/frame.scm")
(install-frame amb-repl-for-install)

(require "q-eval/variable.scm")
(install-variable amb-repl-for-install)

(require "q-eval/expressions.scm")
(install-expressions amb-repl-for-install)

(require "q-eval/database.scm")
(install-database amb-repl-for-install)

(require "q-eval/eval-apply.scm")
(install-eval-apply amb-repl-for-install)

(require "q-eval/repl.scm")
(install-repl amb-repl-for-install)

;; setup repl
(define 
  (make-repl mode)
  (define current-problem '*no-input*)
  (define current-failure-continuation '())
  (define silent (eq? mode 'silent))

  (define 
    (repl input)

    (if (eq? input 'try-again) 
      (begin 
        (if (null? current-failure-continuation) 
          (if (not silent) (displayln ";;; There is no failure continuation"))
          (current-failure-continuation)
        )
      )
      (begin 
        (set! current-problem (list 'repl input))

        (if (not silent) 
          (begin 
            (display ";;; Starting a new problem: ")
            (displayln current-problem)
          )
        )

        (ambeval 
          current-problem
          the-global-environment
          (lambda (val next-alternative) 
            (if (not silent) (displayln val))
            (set! current-failure-continuation next-alternative)
          )
          (lambda () 
            (if (not silent) 
              (begin 
                (display 
                  ";;; There are no more values of: "
                )
                (displayln current-problem)
              )
            )
          )
        )
      )
    )
  )

  repl
)
