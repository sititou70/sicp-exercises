#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "m-eval/eval-apply.scm")
(require "m-eval/global-environment.scm")

(define 
  (make-repl)

  (define current-problem '*no-input*)
  (define current-failure-continuation '())

  (define display-limit 70)
  (define 
    (displaylimitln item)

    (define o (open-output-string))
    (write item o)
    (define str (get-output-string o))

    (if (< display-limit (string-length str)) 
      (begin 
        (display (substring str 0 display-limit))
        (displayln "...")
      )
      (displayln str)
    )
  )

  (define 
    (repl input)

    (if (eq? input 'try-again) 
      (begin 
        (if (null? current-failure-continuation) 
          (displayln ";;; There is no current problem")
          (current-failure-continuation)
        )
      )
      (begin 
        (display ";;; Starting a new problem: ")
        (displaylimitln input)

        (set! current-problem input)
        (ambeval 
          input
          the-global-environment
          (lambda (val next-alternative) 
            (displayln val)
            (set! current-failure-continuation next-alternative)
          )
          (lambda () 
            (display 
              ";;; There are no more values of: "
            )
            (displaylimitln current-problem)
          )
        )
      )
    )
  )

  repl
)
(define repl (make-repl))

; main
(repl 
  '(define
    (require p)
    (if (not p) (amb))
   )
)

(repl 
  '
  (define 
    (an-element-of items)
    (require (not (null? items)))
    (amb (car items) (an-element-of (cdr items)))
  )
)

(repl 
  '
  (define 
    (even? x)
    (= (remainder x 2) 0)
  )
)

(repl 
  '
  (if-fail 
    (let 
      ( ;
       (x (an-element-of '(1 3 5)))
      )

      (require (even? x))
      x
    )
    'all-odd
  )
)
; all-odd

(repl 
  '
  (if-fail 
    (let 
      ( ;
       (x (an-element-of '(1 3 5 8)))
      )

      (require (even? x))
      x
    )
    'all-odd
  )
)
; 8
