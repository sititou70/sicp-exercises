#lang racket
(provide (all-defined-out))

(require sicp)
(require "tag.rkt")

(define 
  (make-error . message)
  (cons 'ec-eval-error message)
)
(define 
  (error? obj)
  (tagged-list? obj 'ec-eval-error)
)
