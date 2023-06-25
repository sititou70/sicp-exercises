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
; 各⼈を各階に割り当てるやり⽅:
; 階の割り当てがそれぞれ別々でない場合は5^5 = 3125通り
; 階の割り当てがそれぞれ別々である場合は5! = 120通り

; 以下では、効率の良い手続きをfast-multiple-dwellingとして示す

(repl 
  '(define
    (require p)
    (sleep 0.001) ; 計測のわかりやすさのためにsleepを設定する
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
      ( ;
       (baker (amb 1 2 3 4 5))
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

(repl 
  '(define
    (fast-multiple-dwelling)
    (let 
      ( ;
       (baker (amb 1 2 3 4 5))
      )

      (require (not (= baker 5)))

      (let 
        ( ;
         (cooper (amb 1 2 3 4 5))
        )

        (require (not (= cooper 1)))
        (require (not (= cooper baker)))

        (let 
          ( ;
           (fletcher (amb 1 2 3 4 5))
          )

          (require (not (= fletcher 5)))
          (require (not (= fletcher 1)))
          (require (not (= (abs (- fletcher cooper)) 1)))
          (require (not (= fletcher baker)))
          (require (not (= fletcher cooper)))

          (let 
            ( ;
             (miller (amb 1 2 3 4 5))
            )

            (require (> miller cooper))
            (require (not (= miller baker)))
            (require (not (= miller cooper)))
            (require (not (= miller fletcher)))

            (let 
              ( ;
               (smith (amb 1 2 3 4 5))
              )

              (require (not (= (abs (- smith fletcher)) 1)))
              (require (not (= smith baker)))
              (require (not (= smith cooper)))
              (require (not (= smith fletcher)))
              (require (not (= smith miller)))

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
; 3607ms

(set! start (current-process-milliseconds))
(repl '(fast-multiple-dwelling))
(repl 'try-again)
(set! end (current-process-milliseconds))
(display (- end start))
(displayln "ms")
; 376ms
