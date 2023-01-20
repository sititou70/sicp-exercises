#lang racket/base

; mermaidコードを生成する
(define (count-change amount) (cc amount 5 "hist"))

(define 
  (cc amount kinds-of-coins hist)
  (cond 
    ((= amount 0) 1)
    ((or (< amount 0) (= kinds-of-coins 0)) 0)
    (else
     (+ 
       (begin 
         (display hist)
         (display "[")
         (display amount)
         (display "_")
         (display kinds-of-coins)
         (display "]")

         (display "--->")

         (display (string-append hist "-0"))
         (display "[")
         (display amount)
         (display "_")
         (display (- kinds-of-coins 1))
         (display "]")
         (display "\n")

         (cc 
           amount
           (- kinds-of-coins 1)
           (string-append hist "-0")
         )
       )

       (begin 
         (display hist)
         (display "[")
         (display amount)
         (display "_")
         (display kinds-of-coins)
         (display "]")

         (display "--->")

         (display (string-append hist "-1"))
         (display "[")
         (display 
           (- amount 
              (first-denomination 
                kinds-of-coins
              )
           )
         )
         (display "_")
         (display kinds-of-coins)
         (display "]")
         (display "\n")

         (cc 
           (- amount 
              (first-denomination 
                kinds-of-coins
              )
           )
           kinds-of-coins
           (string-append hist "-1")
         )
       )
     )
    )
  )
)

(define 
  (first-denomination kinds-of-coins)
  (cond 
    ((= kinds-of-coins 1) 1)
    ((= kinds-of-coins 2) 5)
    ((= kinds-of-coins 3) 10)
    ((= kinds-of-coins 4) 25)
    ((= kinds-of-coins 5) 50)
  )
)

(count-change 11)
