#lang sicp

(define 
  (make-queue)
  (let 
    ((front-ptr '()) 
      (rear-ptr '())
    )

    (define 
      (empty-queue?)
      (null? front-ptr)
    )
    (define 
      (front-queue queue)
      (if (empty-queue?) 
        (error "FRONT called with an empty queue " queue)
        (car front-ptr)
      )
    )

    (define 
      (insert-queue! item)
      (let 
        ((new-pair (cons item '())))
        (cond 
          ((empty-queue?)
           (set! front-ptr new-pair)
           (set! rear-ptr new-pair)
           (cons front-ptr rear-ptr)
          )
          (else
           (set-cdr! rear-ptr new-pair)
           (set! rear-ptr new-pair)
           (cons front-ptr rear-ptr)
          )
        )
      )
    )
    (define 
      (delete-queue!)
      (cond 
        ((empty-queue?)
         (error "DELETE! called with an empty queue")
        )
        (else
         (set! front-ptr (cdr front-ptr))
         (cons front-ptr rear-ptr)
        )
      )
    )

    (define 
      (dispatch m)
      (cond 
        ((eq? m 'empty-queue?) empty-queue?)
        ((eq? m 'front-queue) front-queue)
        ((eq? m 'insert-queue!) insert-queue!)
        ((eq? m 'delete-queue!) delete-queue!)
        (else (error "Unknown request: MAKE-QUEUE" m))
      )
    )
    dispatch
  )
)

(define (insert-queue! q item) ((q 'insert-queue!) item))
(define (delete-queue! q) ((q 'delete-queue!)))

(define (print-queue q) (car q))

(define q1 (make-queue))
(print-queue (insert-queue! q1 'a))
; (a)
(print-queue (insert-queue! q1 'b))
; (a b)
(print-queue (delete-queue! q1))
; (b)
(print-queue (delete-queue! q1))
; ()
