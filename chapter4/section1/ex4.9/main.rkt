#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "m-eval/eval-apply.rkt")
(require "m-eval/global-environment.rkt")

; main
; while構造
; (while condition body)
(eval '(define cnt 0) the-global-environment)
(eval 
  '
  (while (< cnt 5) 
    (displayln cnt)
    (set! cnt (+ cnt 1))
  )
  the-global-environment
)
; 0
; 1
; 2
; 3
; 4
; #f

; for構造
; (for initial-bind condition afterthought body)
(eval 
  '
  (for 
    (i 0)
    (< i 5)
    (set! i (+ i 1))

    (displayln i)
  )
  the-global-environment
)

; 多重ループ
(eval 
  '
  (for 
    (i 0)
    (< i 6)
    (set! i (+ i 1))

    (for 
      (j 0)
      (< j i)
      (set! j (+ j 1))

      (for 
        (k 0)
        (< k j)
        (set! k (+ k 1))

        (displayln (cons k (cons j (cons i '()))))
      )
    )
  )
  the-global-environment
)
; {0 1 2}
; {0 1 3}
; {0 2 3}
; {1 2 3}
; {0 1 4}
; {0 2 4}
; {1 2 4}
; {0 3 4}
; {1 3 4}
; {2 3 4}
; {0 1 5}
; {0 2 5}
; {1 2 5}
; {0 3 5}
; {1 3 5}
; {2 3 5}
; {0 4 5}
; {1 4 5}
; {2 4 5}
; {3 4 5}
; #f
