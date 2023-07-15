#lang racket
(provide (all-defined-out))

(require sicp)
(require "../common/tag.rkt")

; compiled-procedure
(define 
  (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env)
)
(define 
  (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure)
)
(define 
  (compiled-procedure-entry c-proc)
  (cadr c-proc)
)
(define 
  (compiled-procedure-env c-proc)
  (caddr c-proc)
)