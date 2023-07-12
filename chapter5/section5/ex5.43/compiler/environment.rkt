#lang racket
(provide (all-defined-out))

(require sicp)

; environment
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define 
  (make-frame variables values)
  (cons variables values)
)
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define 
  (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame)))
)

; compile time environment
(define 
  (make-compile-time-frame variables)
  (cons variables '())
)
(define 
  (compile-time-frame-variables frame)
  (car frame)
)
(define 
  (add-variable-to-compile-time-frame! var frame)
  (set-car! frame (cons var (car frame)))
)

; lexical address
(define 
  (make-lexical-address frame-number displacement-number)
  (cons frame-number displacement-number)
)
(define 
  (lexical-address-frame-number address)
  (car address)
)
(define 
  (lexical-address-displacement-number address)
  (cdr address)
)

; utils
(define 
  (extend-environment vars vals base-env)
  (if (= (length vars) (length vals)) 
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals)) 
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals)
    )
  )
)
(define 
  (extend-compile-time-environment vars base-env)
  (cons (make-compile-time-frame vars) base-env)
)

(define 
  (lookup-variable-value var env)
  (define 
    (env-loop env)
    (define 
      (scan vars vals)
      (cond 
        ((null? vars)
         (env-loop (enclosing-environment env))
        )
        ((eq? var (car vars)) (car vals))
        (else (scan (cdr vars) (cdr vals)))
      )
    )
    (if (eq? env the-empty-environment) 
      (error "Unbound variable" var)
      (let 
        ((frame (first-frame env)))
        (scan 
          (frame-variables frame)
          (frame-values frame)
        )
      )
    )
  )
  (env-loop env)
)
(define 
  (find-variable var env)

  (define 
    (env-loop env address)

    (define 
      (scan vars address)
      (cond 
        ((null? vars)
         (env-loop 
           (enclosing-environment env)
           (make-lexical-address 
             (+ (lexical-address-frame-number address) 1)
             0
           )
         )
        )
        ((eq? var (car vars)) address)
        (else
         (scan 
           (cdr vars)
           (make-lexical-address 
             (lexical-address-frame-number address)
             (+ (lexical-address-displacement-number address) 1)
           )
         )
        )
      )
    )

    (if (eq? env the-empty-environment) 
      'not-found
      (let 
        ( ;
         (frame (first-frame env))
        )

        (scan 
          (frame-variables frame)
          address
        )
      )
    )
  )

  (env-loop env (make-lexical-address 0 0))
)

(define 
  (set-variable-value! var val env)
  (define 
    (env-loop env)
    (define 
      (scan vars vals)
      (cond 
        ((null? vars)
         (env-loop (enclosing-environment env))
        )
        ((eq? var (car vars)) (set-car! vals val))
        (else (scan (cdr vars) (cdr vals)))
      )
    )
    (if (eq? env the-empty-environment) 
      (error "Unbound variable : SET!" var)
      (let 
        ((frame (first-frame env)))
        (scan 
          (frame-variables frame)
          (frame-values frame)
        )
      )
    )
  )
  (env-loop env)
)

(define 
  (define-variable! var val env)
  (let 
    ( ;
     (frame (first-frame env))
    )

    (define 
      (scan vars vals)
      (cond 
        ((null? vars)
         (add-binding-to-frame! var val frame)
        )
        ((eq? var (car vars)) (set-car! vals val))
        (else (scan (cdr vars) (cdr vals)))
      )
    )

    (scan (frame-variables frame) (frame-values frame))
  )
)
(define 
  (define-compile-time-variable! var env)
  (let 
    ( ;
     (frame (first-frame env))
    )

    (define 
      (scan vars)
      (cond 
        ((null? vars)
         (add-variable-to-compile-time-frame! var frame)
        )
        ((eq? var (car vars)) 'already-defined)
        (else (scan (cdr vars)))
      )
    )

    (scan (compile-time-frame-variables frame))
  )
)

(define 
  (lexical-address-lookup-frame address env)

  (define 
    (lookup-env num env)
    (if (> num 0) 
      (lookup-env (- num 1) (enclosing-environment env))
      env
    )
  )

  (define 
    (lookup-frame num frame)
    (if (> num 0) 
      (lookup-frame 
        (- num 1)
        (make-frame 
          (cdr (frame-variables frame))
          (cdr (frame-values frame))
        )
      )
      frame
    )
  )

  (lookup-frame 
    (lexical-address-displacement-number address)
    (first-frame 
      (lookup-env 
        (lexical-address-frame-number address)
        env
      )
    )
  )
)
(define 
  (lexical-address-lookup address env)
  (let 
    ( ;
     (frame (lexical-address-lookup-frame address env))
    )

    (let 
      ( ;
       (value (car (frame-values frame)))
      )

      (if (eq? value '*unassigned*) 
        (error "Lookup unassigned value")
        value
      )
    )
  )
)
(define 
  (lexical-address-set! address value env)
  (let 
    ( ;
     (frame (lexical-address-lookup-frame address env))
    )

    (set-car! (frame-values frame) value)
  )
)
