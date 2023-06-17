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
  (make-frame bindings filters)
  (cons bindings filters)
)
(define 
  (frame-bindings frame)
  (car frame)
)
(define 
  (frame-filters frame)
  (cdr frame)
)
(define 
  the-empty-frame
  (make-frame '() '())
)

(define 
  (first-filter filters)
  (car filters)
)
(define 
  (rest-filters filters)
  (cdr filters)
)
(define 
  (filters-null? filters)
  (null? filters)
)

(define 
  (make-filter tag operands)
  (cons tag operands)
)
(define 
  (filter-tag filter)
  (car filter)
)
(define 
  (filter-operands filter)
  (cdr filter)
)

(define 
  (binding-in-frame variable frame)
  (assoc variable (frame-bindings frame))
)

(define 
  (extend variable value frame)
  (make-frame 
    (cons 
      (make-binding variable value)
      (frame-bindings frame)
    )
    (frame-filters frame)
  )
)

(define 
  (add-filter frame tag operands)
  (make-frame 
    (frame-bindings frame)
    (cons 
      (make-filter tag operands)
      (frame-filters frame)
    )
  )
)
