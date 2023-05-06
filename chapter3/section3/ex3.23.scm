#lang sicp

(define (make-deque) (cons '() '()))
(define (front-ptr q) (car q))
(define (rear-ptr q) (cdr q))
(define (set-front-ptr! q item) (set-car! q item))
(define (set-rear-ptr! q item) (set-cdr! q item))

(define (make-entry prev next item) (cons (cons prev next) item))
(define (prev-entry e) (caar e))
(define (next-entry e) (cdar e))
(define (item-entry e) (cdr e))
(define (set-prev-entry! e prev) (set-car! (car e) prev))
(define (set-next-entry! e next) (set-cdr! (car e) next))

(define 
  (empty-deque? q)
  (and 
    (null? (front-ptr q))
    (null? (rear-ptr q))
  )
)
(define 
  (front-deque q)
  (if (empty-deque? q) 
    (error "FRONT called with an empty deque" q)
    (item-entry (front-ptr q))
  )
)
(define 
  (rear-deque q)
  (if (empty-deque? q) 
    (error "REAR called with an empty deque" q)
    (item-entry (rear-ptr q))
  )
)

(define 
  (front-insert-deque! q item)
  (let 
    ( ;
     (new-entry 
       (make-entry 
         '()
         (front-ptr q)
         item
       )
     )
    )

    (cond 
      ((empty-deque? q)
       (set-front-ptr! q new-entry)
       (set-rear-ptr! q new-entry)
       q
      )
      (else
       (set-prev-entry! (front-ptr q) new-entry)
       (set-front-ptr! q new-entry)
       q
      )
    )
  )
)
(define 
  (rear-insert-deque! q item)
  (let 
    ( ;
     (new-entry 
       (make-entry 
         (rear-ptr q)
         '()
         item
       )
     )
    )

    (cond 
      ((empty-deque? q)
       (set-front-ptr! q new-entry)
       (set-rear-ptr! q new-entry)
       q
      )
      (else
       (set-next-entry! (rear-ptr q) new-entry)
       (set-rear-ptr! q new-entry)
       q
      )
    )
  )
)

(define 
  (front-delete-deque! q)
  (cond 
    ((empty-deque? q)
     (error "FRONT-DELETE! called with an empty queue" q)
    )
    ((eq? (front-ptr q) (rear-ptr q))
     (set-front-ptr! q '())
     (set-rear-ptr! q '())
     q
    )
    (else
     (let 
       ( ;
        (front (front-ptr q))
        (front-next (next-entry (front-ptr q)))
       )

       (set-next-entry! front '())
       (set-prev-entry! front-next '())
       (set-front-ptr! q front-next)
       q
     )
    )
  )
)
(define 
  (rear-delete-deque! q)
  (cond 
    ((empty-deque? q)
     (error "REAR-DELETE! called with an empty queue" q)
    )
    ((eq? (front-ptr q) (rear-ptr q))
     (set-front-ptr! q '())
     (set-rear-ptr! q '())
     q
    )
    (else
     (let 
       ( ;
        (rear (rear-ptr q))
        (rear-prev (prev-entry (rear-ptr q)))
       )

       (set-prev-entry! rear '())
       (set-next-entry! rear-prev '())
       (set-rear-ptr! q rear-prev)
       q
     )
    )
  )
)

(define 
  (print-deque q)
  (define 
    (iter e)

    (let 
      ( ;
       (last? (eq? e (rear-ptr q)))
      )

      (if (not (null? e)) 
        (begin 
          (display (item-entry e))
          (if (not last?) 
            (display " ")
            'none
          )
        )
        'none
      )
      (if (not last?) 
        (iter (next-entry e))
        'none
      )
    )
  )

  (display "(")
  (iter (front-ptr q))
  (display ")")
  (newline)
)

(define q (make-deque))
(print-deque q)
; ()
(empty-deque? q)
; #t

(print-deque (front-insert-deque! q 'a))
; (a)
(print-deque (front-insert-deque! q 'b))
; (b a)
(print-deque (front-insert-deque! q 'c))
; (c b a)

(empty-deque? q)
; #f
(front-deque q)
; c
(rear-deque q)
; a

(print-deque (rear-delete-deque! q))
; (c b)
(print-deque (rear-delete-deque! q))
; (c)
(print-deque (rear-delete-deque! q))
; ()

(print-deque (rear-insert-deque! q 'a))
; (a)
(print-deque (rear-insert-deque! q 'b))
; (a b)
(print-deque (rear-insert-deque! q 'c))
; (a b c)
(print-deque (front-delete-deque! q))
; (b c)
(print-deque (front-delete-deque! q))
; (c)
(print-deque (front-delete-deque! q))
; ()

(print-deque (front-insert-deque! q '0))
; (0)
(print-deque (rear-insert-deque! q '1))
; (0 1)
(print-deque (front-insert-deque! q '-1))
; (-1 0 1)
(print-deque (rear-insert-deque! q '2))
; (-1 0 1 2)
(print-deque (front-insert-deque! q '-2))
; (-2 -1 0 1 2)
(print-deque (rear-insert-deque! q '3))
; (-2 -1 0 1 2 3)
(print-deque (front-insert-deque! q '-3))
; (-3 -2 -1 0 1 2 3)

(print-deque (front-delete-deque! q))
; (-2 -1 0 1 2 3)
(print-deque (rear-delete-deque! q))
; (-2 -1 0 1 2)
(print-deque (front-delete-deque! q))
; (-1 0 1 2)
(print-deque (front-delete-deque! q))
; (0 1 2)
(print-deque (rear-delete-deque! q))
; (0 1)
(print-deque (rear-delete-deque! q))
; (0)
(print-deque (front-delete-deque! q))
; ()
