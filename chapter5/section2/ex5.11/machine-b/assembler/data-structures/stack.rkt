#lang racket
(provide (all-defined-out))

(require sicp)

(define 
  (make-stack)
  (let 
    ( ;
     (s '())
    )

    (define 
      (push name x)
      (set! s (cons (cons name x) s))
    )
    (define 
      (pop)
      (if (null? s) 
        (error "Empty stack: POP")
        (let 
          ( ;
           (top (car s))
          )

          (set! s (cdr s))
          top
        )
      )
    )
    (define 
      (initialize)
      (set! s '())
      'done
    )
    (define 
      (dispatch message)
      (cond 
        ((eq? message 'push) push)
        ((eq? message 'pop) (pop))
        ((eq? message 'initialize) (initialize))
        (else (error " Unknown request : STACK" message))
      )
    )
    dispatch
  )
)

(define 
  (pop stack)
  (stack 'pop)
)
(define 
  (push stack name value)
  ((stack 'push) name value)
)
(define 
  (stack-item-name item)
  (car item)
)
(define 
  (stack-item-value item)
  (cdr item)
)
