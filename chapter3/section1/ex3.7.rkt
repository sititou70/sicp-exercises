#lang racket/base

(define 
  (make-account balance password)

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
      (error "Incorrect password")
      (cond 
        ((eq? m 'withdraw) withdraw)
        ((eq? m 'deposit) deposit)
        (else (error "Unknown request : MAKE-ACCOUNT" m))
      )
    )
  )
  dispatch
)

(define 
  (make-joint acc old-pass new-pass)
  (define 
    (dispatch p m)
    (if (not (eq? p new-pass)) 
      (error "Incorrect password")
      (acc old-pass m)
    )
  )
  dispatch
)

(define peter-acc (make-account 100 'open-sesame))
; ピーターの操作
((peter-acc 'open-sesame 'withdraw) 30)
; 70
((peter-acc 'open-sesame 'deposit) 50)
; 120

(define 
  paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud)
)
; ポールの操作
((paul-acc 'rosebud 'deposit) 100)
; 220
((paul-acc 'rosebud 'withdraw) 150)
; 70
