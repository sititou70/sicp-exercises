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
         (print hist 

                "["
                amount
                "_"
                kinds-of-coins
                "]"

                "--->"

                (string-append hist "-0")

                "["
                amount
                "_"
                (- kinds-of-coins 1)
                "]"
         )
         (cc 
           amount
           (- kinds-of-coins 1)
           (string-append hist "-0")
         )
       )

       (begin 
         (print hist 

                "["
                amount
                "_"
                kinds-of-coins
                "]"

                "--->"

                (string-append hist "-1")

                "["
                (- amount 
                   (first-denomination 
                     kinds-of-coins
                   )
                )
                "_"
                kinds-of-coins
                "]"
         )
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
