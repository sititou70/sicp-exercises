#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "m-eval/eval-apply.rkt")
(require "m-eval/global-environment.rkt")

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
  '
  (define 
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
    (permanent-set-test)

    (define count 0)
    (let 
      ((x (an-element-of '(a b c))) 
        (y (an-element-of '(a b c)))
      )
      (permanent-set! count (+ count 1))
      (require (not (eq? x y)))
      (list x y count)
    )
  )
)
(repl '(permanent-set-test))
(repl 'try-again)
; {a b 2}
; {a c 3}

(repl 
  '
  (define 
    (set-test)

    (define count 0)
    (let 
      ((x (an-element-of '(a b c))) 
        (y (an-element-of '(a b c)))
      )
      (set! count (+ count 1))
      (require (not (eq? x y)))
      (list x y count)
    )
  )
)
(repl '(set-test))
(repl 'try-again)
; {a b 1}
; {a c 1}
