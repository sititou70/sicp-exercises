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

; xorの真理値表を見ると
; p1  p2  xor
; t   t   f
; t   f   t
; f   t   t
; f   f   f
; p1がtのとき、xorはnot p2、p1がfのときxorはp2であると気づく
; from: http://community.schemewiki.org/?sicp-ex-4.42
(repl 
  '(define
    (xor p1 p2)
    (if p1 
      (not p2)
      p2
    )
   )
)

(repl 
  '(define
    (liars-puzzle)

    (let 
      ( ;
       (betty (amb 1 2 3 4 5))
       (ethel (amb 1 2 3 4 5))
       (joan (amb 1 2 3 4 5))
       (kitty (amb 1 2 3 4 5))
       (mary (amb 1 2 3 4 5))
      )

      (require (xor (= kitty 2) (= betty 3)))
      (require (xor (= ethel 1) (= joan 2)))
      (require (xor (= joan 3) (= ethel 5)))
      (require (xor (= kitty 2) (= mary 4)))
      (require (xor (= mary 4) (= betty 1)))
      (require (distinct? (list betty ethel joan kitty mary)))
      (list 
        (list 'betty betty)
        (list 'ethel ethel)
        (list 'joan joan)
        (list 'kitty kitty)
        (list 'mary mary)
      )
    )
   )
)

(repl '(liars-puzzle))
(repl 'try-again)
; {{betty 3} {ethel 5} {joan 2} {kitty 1} {mary 4}}
;;; There are no more values of: (liars-puzzle)
