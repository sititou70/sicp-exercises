#lang racket
(provide (all-defined-out))

(require sicp)

(define 
  (shuffle items)

  (define 
    (pick items index)

    (define 
      (iter rest cnt picked unpicked)
      (cond 
        ((null? rest) (cons picked unpicked))
        ((= cnt index)
         (iter 
           (cdr rest)
           (+ cnt 1)
           (car rest)
           unpicked
         )
        )
        (else
         (iter 
           (cdr rest)
           (+ cnt 1)
           picked
           (cons (car rest) unpicked)
         )
        )
      )
    )

    (iter items 0 '() '())
  )

  (define 
    (iter rest shuffled)
    (if (null? rest) 
      shuffled
      (let 
        ( ;
         (pick-result (pick rest (random (length rest))))
        )

        (iter (cdr pick-result) (cons (car pick-result) shuffled))
      )
    )
  )

  (iter items '())
)
