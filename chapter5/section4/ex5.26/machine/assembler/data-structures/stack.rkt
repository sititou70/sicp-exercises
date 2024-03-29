#lang racket
(provide (all-defined-out))

(require sicp)

(define 
  (make-stack)
  (let 
    ( ;
     (s '())
     (number-pushes 0)
     (max-depth 0)
     (current-depth 0)
    )

    (define 
      (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth))
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
          (set! current-depth (- current-depth 1))
          top
        )
      )
    )
    (define 
      (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done
    )
    (define 
      (print-statistics)
      (displayln 
        (list 'total-pushes '= number-pushes 'maximum-depth '= max-depth)
      )
    )
    (define 
      (dispatch message)
      (cond 
        ((eq? message 'push) push)
        ((eq? message 'pop) (pop))
        ((eq? message 'initialize) (initialize))
        ((eq? message 'print-statistics) (print-statistics))
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
  (push stack value)
  ((stack 'push) value)
)
