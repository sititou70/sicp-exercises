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
; 練習問題 4.35の⼿続き内の an-integer-between を単純に an-integer-starting-from で置き換えるというやり⽅が、任意のピタゴラス数を⽣成する⽅法としてなぜ適切でないのか説明せよ。
; 上記の方法では、jは常に失敗しないため、iは1のままであり、任意のピタゴラス数を生成するとは言えない

(repl 
  '(define
    (require p)
    (if (not p) (amb))
   )
)

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
    (an-integer-starting-from from)

    (amb 
      from
      (an-integer-starting-from 
        (+ from 1)
      )
    )
   )
)

; a^2 + b^2 = c^2
; ここで、
; a^2 = c^2 - b^2
; a^2 <= c^2
; a <= c
; 同様に
; b <= c
; また、ex4.35の問題文よりa <= bであるから
(repl 
  '(define
    (a-pythagorean-triple)
    (let 
      ( ;
       (c (an-integer-starting-from 1))
      )

      (let 
        ( ;
         (a (an-integer-between 1 c))
        )

        (let 
          ( ;
           (b (an-integer-between a c))
          )

          (require (= (+ (* a a) (* b b)) (* c c)))
          (list a b c)
        )
      )
    )
   )
)

(repl '(a-pythagorean-triple))
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
; {3 4 5}
; {6 8 10}
; {5 12 13}
; {9 12 15}
; {8 15 17}
; {12 16 20}
; {7 24 25}
; {15 20 25}
; {10 24 26}
; {20 21 29}
; {18 24 30}
