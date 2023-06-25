#lang racket/base
(provide (all-defined-out))

(require sicp)

; see: https://github.com/racket/srfi/blob/25eb1c0e1ab8a1fa227750aa7f0689a2c531f8c8/srfi-lite-lib/srfi/1/predicate.rkt#LL86C1-L89C64
(define 
  (null-list? l)
  (cond 
    ((pair? l) #f)
    ((null? l) #t)
    (else (error "null-list?: argument out of domain" l))
  )
)

; see: https://github.com/racket/srfi/blob/25eb1c0e1ab8a1fa227750aa7f0689a2c531f8c8/srfi-lite-lib/srfi/1/fold.rkt#LL77C1-L86C50
(define 
  (fold kons knil lis1 . lists)
  (let 
    lp
    ((lis lis1) (ans knil)) ; Fast path
    (if (null-list? lis) 
      ans
      (lp (cdr lis) (kons (car lis) ans))
    )
  )
)
