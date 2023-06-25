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
            (set! current-failure-continuation next-alternative)
            val
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
    (or p1 p2)
    (if p1 
      true
      (if p2 
        true
        false
      )
    )
  )
)

(repl 
  '
  (define 
    (diagonal? p1 p2)
    (let 
      ( ;
       (x1 (car p1))
       (y1 (cdr p1))
       (x2 (car p2))
       (y2 (cdr p2))
      )

      (let 
        ( ;
         (slope (/ (- y2 y1) (- x2 x1)))
        )

        (or (= slope 1) (= slope -1))
      )
    )
  )
)

(repl 
  '
  (define 
    (horizontal? p1 p2)
    (let 
      ( ;
       (y1 (cdr p1))
       (y2 (cdr p2))
      )

      (= y1 y2)
    )
  )
)

(repl 
  '
  (define 
    (vertical? p1 p2)
    (let 
      ( ;
       (x1 (car p1))
       (x2 (car p2))
      )

      (= x1 x2)
    )
  )
)

(repl 
  '
  (define 
    (safe? positions new-position)


    (cond 
      ((null? positions) true)
      ((diagonal? (car positions) new-position) false)
      ((horizontal? (car positions) new-position) false)
      ((vertical? (car positions) new-position) false)
      (else (safe? (cdr positions) new-position))
    )
  )
)

(repl 
  '
  (define 
    (queens)

    (define p1 (cons 1 (amb 1 2 3 4 5 6 7 8)))

    (define p2 (cons 2 (amb 1 2 3 4 5 6 7 8)))
    (require (safe? (list p1) p2))

    (define p3 (cons 3 (amb 1 2 3 4 5 6 7 8)))
    (require (safe? (list p1 p2) p3))

    (define p4 (cons 4 (amb 1 2 3 4 5 6 7 8)))
    (require (safe? (list p1 p2 p3) p4))

    (define p5 (cons 5 (amb 1 2 3 4 5 6 7 8)))
    (require (safe? (list p1 p2 p3 p4) p5))

    (define p6 (cons 6 (amb 1 2 3 4 5 6 7 8)))
    (require (safe? (list p1 p2 p3 p4 p5) p6))

    (define p7 (cons 7 (amb 1 2 3 4 5 6 7 8)))
    (require (safe? (list p1 p2 p3 p4 p5 p6) p7))

    (define p8 (cons 8 (amb 1 2 3 4 5 6 7 8)))
    (require (safe? (list p1 p2 p3 p4 p5 p6 p7) p8))

    (list p1 p2 p3 p4 p5 p6 p7 p8)
  )
)

(define 
  (print-board positions)
  (map 
    (lambda (x) 
      (map 
        (lambda (y) 
          (if (member (cons x y) positions) 
            (display "Q")
            (display ".")
          )
        )
        '(1 2 3 4 5 6 7 8)
      )
      (newline)
    )
    '(1 2 3 4 5 6 7 8)
  )
  (newline)
)

(print-board (repl '(queens)))
; Q.......
; ....Q...
; .......Q
; .....Q..
; ..Q.....
; ......Q.
; .Q......
; ...Q....

(print-board (repl 'try-again))
; Q.......
; .....Q..
; .......Q
; ..Q.....
; ......Q.
; ...Q....
; .Q......
; ....Q...
