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
    (prime? n)

    (define 
      (square x)
      (* x x)
    )

    (define (divides? a b) (= (remainder b a) 0))

    (define 
      (find-divisor n test-divisor)
      (cond 
        ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))
      )
    )

    (define (smallest-divisor n) (find-divisor n 2))

    (= n (smallest-divisor n))
  )
)

(repl 
  '
  (define 
    (prime-sum-pair list1 list2)
    (let 
      ( ;
       (a (an-element-of list1))
       (b (an-element-of list2))
      )

      (require (prime? (+ a b)))
      (list a b)
    )
  )
)

(repl 
  '
  (let 
    ( ;
     (pairs '())
    )

    (if-fail 
      (let 
        ( ;
         (p 
           (prime-sum-pair 
             '(1 3 5 8)
             '(20 35 110)
           )
         )
        )

        (permanent-set! pairs (cons p pairs))
        (amb)
      )
      pairs
    )
  )
)
; {{8 35} {3 110} {3 20}}

; 和が素数になる数のペアをすべて列挙する
; permanent-set!と(amb)によりprime-sum-pairが生成するリストをループ、集積し、if-failによって結果を返している
