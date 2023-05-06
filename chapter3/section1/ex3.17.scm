#lang sicp

(define 
  (count-pairs x)

  (define 
    (search x found-pairs)

    (if 
      (and 
        (pair? x)
        (not (memq x found-pairs))
      )

      (let 
        ( ;
         (car-result (search (car x) (cons x found-pairs)))
        )

        (append 
          (list x)
          car-result
          (search (cdr x) (append (list x) car-result found-pairs))
        )
      )

      '()
    )
  )

  (length (search x '()))
)

(define l1 (cons 1 (cons 2 (cons 3 nil))))
(count-pairs l1)
; 3

(define l2-1 (cons '1 '2))
(define l2-2 (cons l2-1 l2-1))
(define l2 (cons l2-2 '3))
(count-pairs l2)
; 3

(define l3-1 (cons '1 '2))
(define l3-2 (cons l3-1 l3-1))
(define l3 (cons l3-2 l3-2))
(count-pairs l3)
; 3

(define l4 (cons 1 (cons 2 (cons 3 nil))))
(set-cdr! (cddr l4) l4)
(count-pairs l4)
; 3
