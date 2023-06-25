#lang racket
(provide (all-defined-out))

(require sicp)

; see: http://community.schemewiki.org/?sicp-ex-2.73
(define *table* (make-hash))
(define (put key1 key2 value) (hash-set! *table* (list key1 key2) value))
(define (get key1 key2) (hash-ref *table* (list key1 key2) #f))

(define 
  (type exp)
  (if (pair? exp) 
    (car exp)
    (error "Unknown expression TYPE" exp)
  )
)
(define 
  (contents exp)
  (if (pair? exp) 
    (cdr exp)
    (error "Unknown expression CONTENTS" exp)
  )
)

(define 
  (tagged-list? exp tag)
  (if (pair? exp) 
    (eq? (car exp) tag)
    false
  )
)
