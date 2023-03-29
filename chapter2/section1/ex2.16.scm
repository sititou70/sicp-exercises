#lang racket/base

(define (make-interval a b) (cons a b))
(define (lower-bound a) (car a))
(define (upper-bound a) (cdr a))

(define 
  (make-center-percent c p)
  (make-interval (- c (* c (/ p 100))) (+ c (* c (/ p 100))))
)
(define 
  (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2)
)
(define 
  (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2)
)
(define 
  (percent i)
  (* (/ (width i) (center i)) 100)
)

(define 
  (par1 r1 r2)
  (/ 
    (* r1 r2)
    (+ r1 r2)
  )
)
(define 
  (par2 r1 r2)
  (/ 
    1
    (+ 
      (/ 1 r1)
      (/ 1 r2)
    )
  )
)

; 2つの区間と2変数を含む式をうけとり、それを評価した結果の区間を返す
(define 
  (eval-2var i1 i2 expr)

  (let 
    ((r1 (expr (lower-bound i1) (lower-bound i2))) 
      (r2 (expr (lower-bound i1) (upper-bound i2)))
      (r3 (expr (upper-bound i1) (lower-bound i2)))
      (r4 (expr (upper-bound i1) (upper-bound i2)))
    )
    (make-interval 
      (min r1 r2 r3 r4)
      (max r1 r2 r3 r4)
    )
  )
)

(eval-2var (make-center-percent 6.8 10) (make-center-percent 4.7 5) par1)
(eval-2var (make-center-percent 6.8 10) (make-center-percent 4.7 5) par2)
; '(2.581558809636278 . 2.97332259363673)
; '(2.581558809636278 . 2.97332259363673)
; 同じ結果が得られた
