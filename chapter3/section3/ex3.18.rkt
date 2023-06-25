#lang sicp

(define 
  (loop? x)

  (define 
    (search x visited-pairs)

    (cond 
      ((not (pair? x)) #f)
      ((memq x visited-pairs) #t)
      (else (search (cdr x) (cons x visited-pairs)))
    )
  )

  (search x '())
)

(define l1 (cons 1 (cons 2 (cons 3 nil))))
(loop? l1)
; #f

(define l4 (cons 1 (cons 2 (cons 3 nil))))
(set-cdr! (cddr l4) l4)
(loop? l4)
; #t
