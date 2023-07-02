#lang racket
(provide (all-defined-out))

(require sicp)

(define 
  (tagged-list? exp tag)
  (if (pair? exp) 
    (eq? (car exp) tag)
    false
  )
)
