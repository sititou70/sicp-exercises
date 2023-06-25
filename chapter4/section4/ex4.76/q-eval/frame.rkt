#lang racket
(provide (all-defined-out))

(require sicp)

(define 
  (make-binding variable value)
  (cons variable value)
)
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))

(define 
  (binding-in-frame variable frame)
  (assoc variable frame)
)

(define 
  (extend variable value frame)
  (cons (make-binding variable value) frame)
)

(define 
  (first-binding frame)
  (car frame)
)
(define 
  (rest-bindings frame)
  (cdr frame)
)
