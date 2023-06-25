#lang racket/base

(define 
  (variable? x)
  (symbol? x)
)
(define 
  (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2))
)

(define 
  (=number? exp num)
  (and (number? exp) (= exp num))
)

(define 
  (extract-value x)
  (if (= (length x) 1) 
    (car x)
    x
  )
)

(define 
  (make-sum a1 a2)
  (cond 
    ((=number? a1 0) a2)
    ((=number? a2 0) a1)
    ((and (number? a1) (number? a2)) (+ a1 a2))
    (else (list a1 '+ a2))
  )
)
(define 
  (sum? x)
  (and (pair? x) (memq '+ x))
)
(define 
  (addend s)

  (define 
    (iter result rest)
    (cond 
      ((null? rest) '())
      ((eq? (car rest) '+) result)
      (else
       (iter 
         (append result (list (car rest)))
         (cdr rest)
       )
      )
    )
  )

  (extract-value (iter '() s))
)
(define 
  (augend s)
  (extract-value (cdr (memq '+ s)))
)

(define 
  (make-product m1 m2)
  (cond 
    ((or (=number? m1 0) (=number? m2 0)) 0)
    ((=number? m1 1) m2)
    ((=number? m2 1) m1)
    ((and (number? m1) (number? m2)) (* m1 m2))
    (else (list m1 '* m2))
  )
)
(define 
  (product? x)
  (not (sum? x))
)
(define 
  (multiplier p)
  (car p)
)
(define 
  (multiplicand p)
  (extract-value (cdr (memq '* p)))
)

(define 
  (deriv exp var)
  (cond 
    ((number? exp) 0)
    ((variable? exp) (if (same-variable? exp var) 1 0))
    ((sum? exp)
     (make-sum 
       (deriv (addend exp) var)
       (deriv (augend exp) var)
     )
    )
    ((product? exp)
     (make-sum 
       (make-product 
         (multiplier exp)
         (deriv (multiplicand exp) var)
       )
       (make-product 
         (deriv (multiplier exp) var)
         (multiplicand exp)
       )
     )
    )
    (else
     (error " unknown expression type: DERIV " exp)
    )
  )
)

(deriv '(x + 3 * (x + y + 2)) 'x)
; 4
