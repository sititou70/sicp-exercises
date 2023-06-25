#lang racket
(provide (all-defined-out))

(require sicp)

(define (make-frame bindings last-rule) (cons bindings last-rule))
(define 
  the-empty-frame
  (make-frame '() '())
)
(define (frame-bindings f) (car f))
(define (frame-last-rule f) (cdr f))

(define 
  (make-binding variable value)
  (cons variable value)
)
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))

(define 
  (binding-in-frame variable frame)
  (assoc variable (frame-bindings frame))
)

(define 
  (extend variable value frame)
  (make-frame 
    (cons (make-binding variable value) (frame-bindings frame))
    (frame-last-rule frame)
  )
)

(define 
  (register-last-rule frame last-rule)
  (make-frame (frame-bindings frame) last-rule)
)
