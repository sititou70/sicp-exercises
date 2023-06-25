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
  (make-sum . args)
  (let 
    ( ;
     (coefficient 
       (apply + (filter number? args))
     )
     (terms 
       (filter (lambda (x) (not (number? x))) args)
     )
    )
    (let 
      ( ;
       (filterd 
         (filter 
           (lambda (x) (not (=number? x 0)))
           (append '(+) terms (list coefficient))
         )
       )
      )
      (cond 
        ((= (length filterd) 1) 0)
        ((= (length filterd) 2) (cadr filterd))
        (else filterd)
      )
    )
  )
)
(define 
  (sum? x)
  (and (pair? x) (eq? (car x) '+))
)
(define 
  (addend s)
  (cadr s)
)
(define 
  (augend s)
  (let 
    ((rest (cddr s)))
    (if (= (length rest) 1) 
      (car rest)
      (apply make-sum rest)
    )
  )
)

(define 
  (make-product . args)
  (if (memq 0 args) 
    0
    (let 
      ( ;
       (coefficient 
         (apply * (filter number? args))
       )
       (terms 
         (filter (lambda (x) (not (number? x))) args)
       )
      )
      (let 
        ( ;
         (filterd 
           (filter 
             (lambda (x) (not (=number? x 1)))
             (append '(*) (list coefficient) terms)
           )
         )
        )
        (cond 
          ((= (length filterd) 1) 1)
          ((= (length filterd) 2) (cadr filterd))
          (else filterd)
        )
      )
    )
  )
)
(define 
  (product? x)
  (and (pair? x) (eq? (car x) '*))
)
(define 
  (multiplier p)
  (cadr p)
)
(define 
  (multiplicand p)
  (let 
    ((rest (cddr p)))
    (if (= (length rest) 1) 
      (car rest)
      (apply make-product rest)
    )
  )
)

(define 
  (make-exponentiation base exponent)
  (cond 
    ((=number? exponent 0) 1)
    ((=number? exponent 1) base)
    (else (list '** base exponent))
  )
)
(define 
  (exponentiation? x)
  (and (pair? x) (eq? (car x) '**))
)
(define 
  (base e)
  (cadr e)
)
(define 
  (exponent e)
  (caddr e)
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
    ((exponentiation? exp)
     (make-product 
       (exponent exp)
       (make-product 
         (make-exponentiation (base exp) (- (exponent exp) 1))
         (deriv (base exp) var)
       )
     )
    )
    (else
     (error " unknown expression type: DERIV " exp)
    )
  )
)

(deriv '(+ 1 2 3 x) 'x)
; 1
(deriv '(+ 0 x) 'x)
; 1

(deriv '(* 1 2 3 x) 'x)
; 6
(deriv '(* 0 x) 'x)
; 0

(deriv '(* x y (+ x 3)) 'x)
; '(+ (* x y) (* y (+ x 3)))

; (x**2 + y**2 + 3x) ** 3
(deriv 
  '(**
    (+ (** x 2) 
       (** y 2)
       (* 3 x)
    )
    3
   )
  'x
)
; '(* 3 (* (** (+ (** x 2) (** y 2) (* 3 x)) 2) (+ (* 2 x) 3)))
; = 3 ((x**2 + y**2 + 3x) ** 2) (2x + 3)
