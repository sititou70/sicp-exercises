#lang racket
(provide (all-defined-out))

(require sicp)

; environment
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define 
  (make-frame binds)
  (list binds)
)
(define (frame-binds frame) (car frame))
(define 
  (add-binding-to-frame! bind frame)
  (set-car! frame (cons bind (frame-binds frame)))
)

; utils
(define 
  (extend-environment binds base-env)
  (cons (make-frame binds) base-env)
)

(define 
  (lookup-variable-value var env)
  (define 
    (env-loop env)
    (define 
      (scan binds)
      (cond 
        ((null? binds)
         (env-loop (enclosing-environment env))
        )
        ((eq? var (caar binds)) (cdar binds))
        (else (scan (cdr binds)))
      )
    )
    (if (eq? env the-empty-environment) 
      (error "Unbound variable" var)
      (let 
        ( ;
         (frame (first-frame env))
        )

        (scan 
          (frame-binds frame)
        )
      )
    )
  )
  (env-loop env)
)

(define 
  (set-variable-value! bind env)
  (let 
    ( ;
     (var (car bind))
     (val (cdr bind))
    )

    (define 
      (env-loop env)
      (define 
        (scan binds)
        (cond 
          ((null? binds)
           (env-loop (enclosing-environment env))
          )
          ((eq? var (caar binds)) (set-cdr! (car binds) val))
          (else (scan (cdr binds)))
        )
      )
      (if (eq? env the-empty-environment) 
        (error "Unbound variable : SET!" var)
        (let 
          ( ;
           (frame (first-frame env))
          )

          (scan 
            (frame-binds frame)
          )
        )
      )
    )
    (env-loop env)
  )
)

(define 
  (define-variable! bind env)
  (let 
    ( ;
     (frame (first-frame env))
     (var (car bind))
     (val (cdr bind))
    )

    (define 
      (scan binds)
      (cond 
        ((null? binds)
         (add-binding-to-frame! bind frame)
        )
        ((eq? var (caar binds)) (set-cdr! (car binds) val))
        (else (scan (cdr binds)))
      )
    )

    (scan (frame-binds frame))
  )
)
