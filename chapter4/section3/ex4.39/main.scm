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

; 制約の並び順は解に影響しない。各変数の探索順序は制約の順番によって変化しないため、解の個数や順序も変わらないからである。

; 制約の並び順は実行速度に影響する。
; 探索するすべてのパターンについて、より偽となることが多く、また判定速度が早い制約を先に実行することで、後の制約の実行を打ち切ることができるからである。
; 例えば、distinct?は他の制約に比べてリストを探索しなければならないため、時間がかかると仮定する。
; なお、計測を簡単にするためにdistinct?に1ミリ秒のsleepを設定する

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
    (orig-multiple-dwelling)
    (let 
      ((baker (amb 1 2 3 4 5)) 
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5))
      )
      (sleep 0.001)
      (require (distinct? (list baker cooper fletcher miller smith)))
      (require (not (= baker 5)))
      (require (not (= cooper 1)))
      (require (not (= fletcher 5)))
      (require (not (= fletcher 1)))
      (require (> miller cooper))
      (require (not (= (abs (- smith fletcher)) 1)))
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

(define start 0)
(define end 0)

(set! start (current-process-milliseconds))
(repl '(orig-multiple-dwelling))
(repl 'try-again)
(set! end (current-process-milliseconds))
(display (- end start))
(displayln "ms")
; 3183ms

(repl 
  '(define
    (fast-multiple-dwelling)
    (let 
      ((baker (amb 1 2 3 4 5)) 
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5))
      )
      (require (not (= baker 5)))
      (require (not (= cooper 1)))
      (require (not (= fletcher 5)))
      (require (not (= fletcher 1)))
      (require (> miller cooper))
      (require (not (= (abs (- smith fletcher)) 1)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (sleep 0.001)
      (require (distinct? (list baker cooper fletcher miller smith)))
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

(set! start (current-process-milliseconds))
(repl '(fast-multiple-dwelling))
(repl 'try-again)
(set! end (current-process-milliseconds))
(display (- end start))
(displayln "ms")
; 126ms
