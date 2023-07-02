#lang racket
(provide (all-defined-out))

(require sicp)
(require "tag.rkt")

; constant
(define 
  (constant-exp? exp)
  (tagged-list? exp 'const)
)
(define 
  (constant-exp-value exp)
  (cadr exp)
)

; assign
(define 
  (assign-reg-name assign-instruction)
  (cadr assign-instruction)
)
(define 
  (assign-value-exp assign-instruction)
  (cddr assign-instruction)
)

; test
(define 
  (test-condition test-instruction)
  (cdr test-instruction)
)

; branch
(define 
  (branch-dest branch-instruction)
  (cadr branch-instruction)
)

; goto
(define 
  (goto-dest goto-instruction)
  (cadr goto-instruction)
)

; save and restore
(define 
  (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction)
)

; perform
(define 
  (perform-action inst)
  (cdr inst)
)

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))

; label
(define 
  (label-exp? exp)
  (tagged-list? exp 'label)
)
(define 
  (label-exp-label exp)
  (cadr exp)
)

; operation
; example: ((op rem) (reg a) (reg b))
(define 
  (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op))
)
(define 
  (operation-exp-op operation-exp)
  (cadr (car operation-exp))
)
(define 
  (operation-exp-operands operation-exp)
  (cdr operation-exp)
)
