#lang sicp

; 参考：https://en.wikipedia.org/wiki/Cycle_detection#Floyd's_tortoise_and_hare
(define 
  (loop? x)

  (define 
    (safe-cdr x)
    (if (pair? x) (cdr x) 'fail)
  )

  (define 
    (search fast slow)
    (cond 
      ((not (pair? fast)) #f)
      ((not (pair? slow)) #f)
      ((eq? fast slow) #t)
      (else (search (safe-cdr (safe-cdr fast)) (safe-cdr slow)))
    )
  )

  (search (safe-cdr x) x)
)

(loop? '())
; #f
(loop? '(1))
; #f
(loop? '(1 2))
; #f
(loop? '(1 2 3))
; #f
(loop? '(1 2 3 4))
; #f
(loop? '(1 2 3 4 5))
; #f
(loop? '(1 2 3 4 5 6))
; #f
(loop? '(1 2 3 4 5 6 7))
; #f
(loop? '(1 2 3 4 5 6 7 8))
; #f

(define l4 (cons 1 (cons 2 (cons 3 nil))))
(set-cdr! (cddr l4) l4)
(loop? l4)
; #t
