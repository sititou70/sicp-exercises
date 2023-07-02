#lang racket
(provide (all-defined-out))

(require sicp)
(require "tag.rkt")

(define 
  (delay-it exp env)
  (list 'thunk exp env)
)
(define 
  (thunk? obj)
  (tagged-list? obj 'thunk)
)
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define 
  (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk)
)
(define 
  (thunk-value evaluated-thunk)
  (cadr evaluated-thunk)
)
(define 
  (set-evaluated-thunk! thunk result)
  (set-car! thunk 'evaluated-thunk)
  (set-car! (cdr thunk) result)
  (set-cdr! (cdr thunk) '())
)
