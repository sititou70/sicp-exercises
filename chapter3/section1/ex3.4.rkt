#lang racket/base

(define 
  (call-the-cops)
  (newline)
  (display "!! 👮<警察だ !!")
  (newline)
)

(define 
  (make-account balance password)

  (define try-incorrect-pass-count 0)

  (define 
    (withdraw amount)
    (if (>= balance amount) 
      (begin 
        (set! balance (- balance amount))
        balance
      )
      "Insufficient funds"
    )
  )
  (define 
    (deposit amount)
    (set! balance (+ balance amount))
    balance
  )

  (define 
    (dispatch p m)
    (if (not (eq? p password)) 
      (begin 
        (set! try-incorrect-pass-count (+ try-incorrect-pass-count 1))
        (if (= try-incorrect-pass-count 7) (call-the-cops) 'none)
        "Incorrect password"
      )
      (cond 
        ((eq? m 'withdraw) withdraw)
        ((eq? m 'deposit) deposit)
        (else
         (error 
           "Unknown request : MAKE-ACCOUNT"
           m
         )
        )
      )
    )
  )
  dispatch
)

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
; 60

(acc 'some-other-password 'deposit)
; "Incorrect password"
(acc 'some-other-password 'deposit)
; "Incorrect password"
(acc 'some-other-password 'deposit)
; "Incorrect password"
(acc 'some-other-password 'deposit)
; "Incorrect password"
(acc 'some-other-password 'deposit)
; "Incorrect password"
(acc 'some-other-password 'deposit)
; "Incorrect password"
(acc 'some-other-password 'deposit)
; "Incorrect password"
