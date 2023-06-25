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
  '(define
    (an-integer-between low high)

    (require (<= low high))
    (amb 
      low
      (an-integer-between 
        (+ low 1)
        high
      )
    )
   )
)

(repl 
  '(define
    (a-pythagorean-triple-between low high)
    (let 
      ( ;
       (i (an-integer-between low high))
      )

      (let 
        ( ;
         (j (an-integer-between i high))
        )

        (let 
          ( ;
           (k (an-integer-between j high))
          )

          (require (= (+ (* i i) (* j j)) (* k k)))
          (list i j k)
        )
      )
    )
   )
)

(repl '(a-pythagorean-triple-between 1 20))
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
; {3 4 5}
; {5 12 13}
; {6 8 10}
; {8 15 17}
; {9 12 15}
; {12 16 20}
; ;;; There are no more values of: (a-pythagorean-triple-between 1 20)
