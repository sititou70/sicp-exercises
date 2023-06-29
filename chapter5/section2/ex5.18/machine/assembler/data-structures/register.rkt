#lang racket
(provide (all-defined-out))

(require sicp)

(define 
  (make-register name)
  (let 
    ( ;
     (contents '*unassigned*)
     (trace? false)
    )

    (define 
      (dispatch message)
      (cond 
        ((eq? message 'get) contents)
        ((eq? message 'set)
         (lambda (value) 
           (if trace? 
             (begin 
               (display name)
               (display ": ")
               (display contents)
               (display " -> ")
               (displayln value)
             )
           )
           (set! contents value)
         )
        )
        ((eq? message 'trace) (set! trace? true))
        ((eq? message 'untrace) (set! trace? false))
        (else
         (error "Unknown request: REGISTER" message)
        )
      )
    )
    dispatch
  )
)

(define 
  (get-contents register)
  (register 'get)
)
(define 
  (set-contents! register value)
  ((register 'set) value)
)
