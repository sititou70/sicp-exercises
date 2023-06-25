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

; utils
(define 
  (extend-environment vars vals base-env)
  (if (= (length vars) (length vals)) 
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals)) 
      (error "Too many arguments supplied " vars vals)
      (error "Too few arguments supplied " vars vals)
    )
  )
)

; envからvarを探索し、varが見つかった時点での(vars vals env)を返す。varが見つからなければ、戻り値のvarとvalはnullになる
(define 
  (lookup-variable var env)
  (define 
    (env-loop env)
    (define 
      (scan vars vals)
      (cond 
        ((null? vars)
         (env-loop (enclosing-environment env))
        )
        ((eq? var (car vars)) (list vars vals env))
        (else (scan (cdr vars) (cdr vals)))
      )
    )
    (if (eq? env the-empty-environment) 
      (list '() '() env)
      (let 
        ( ;
         (frame (first-frame env))
        )

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
  (lookup-variable-value var env)
  (let 
    ( ;
     (res (lookup-variable var env))
    )

    (if (null? (car res)) 
      (error "Unbound variable" var)
      (caadr res)
    )
  )
)

(define 
  (set-variable-value! var val env)
  (let 
    ( ;
     (res (lookup-variable var env))
    )

    (if (null? (car res)) 
      (error "Unbound variable: SET!" var)
      (set-car! (cadr res) val)
    )
  )
)

(define 
  (define-variable! var val env)
  (let 
    ( ;
     (res (lookup-variable var env))
    )

    (if (null? (car res)) 
      (add-binding-to-frame! var val (first-frame env))
      (set-car! (cadr res) val)
    )
  )
)

