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
    (require p)
    (if (not p) (amb))
   )
)

(repl 
  '(define
    (distinct? items)
    (cond 
      ((null? items) true)
      ((null? (cdr items)) true)
      ((member (car items) (cdr items)) false)
      (else (distinct? (cdr items)))
    )
   )
)

(repl 
  '(define
    (multiple-dwelling)
    (let 
      ((baker (amb 1 2 3 4 5)) 
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5))
      )
      (require (distinct? (list baker cooper fletcher miller smith)))
      (require (not (= baker 5)))
      (require (not (= cooper 1)))
      (require (not (= fletcher 5)))
      (require (not (= fletcher 1)))
      (require (> miller cooper))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (list 
        (list 'baker baker)
        (list 'cooper cooper)
        (list 'fletcher fletcher)
        (list 'miller miller)
        (list 'smith smith)
      )
    )
   )
)

(repl '(multiple-dwelling))
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
; {{baker 1} {cooper 2} {fletcher 4} {miller 3} {smith 5}}
; {{baker 1} {cooper 2} {fletcher 4} {miller 5} {smith 3}}
; {{baker 1} {cooper 4} {fletcher 2} {miller 5} {smith 3}}
; {{baker 3} {cooper 2} {fletcher 4} {miller 5} {smith 1}}
; {{baker 3} {cooper 4} {fletcher 2} {miller 5} {smith 1}}
;;; There are no more values of: (multiple-dwelling)

; 5つの解がある
