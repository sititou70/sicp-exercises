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
  (add-interval x y)
  (make-interval 
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))
  )
)
(define 
  (mul-interval x y)
  (let 
    ((p1 (* (lower-bound x) (lower-bound y))) 
      (p2 (* (lower-bound x) (upper-bound y)))
      (p3 (* (upper-bound x) (lower-bound y)))
      (p4 (* (upper-bound x) (upper-bound y)))
    )
    (make-interval 
      (min p1 p2 p3 p4)
      (max p1 p2 p3 p4)
    )
  )
)
(define 
  (div-interval x y)
  (mul-interval 
    x
    (make-interval 
      (/ 1.0 (upper-bound y))
      (/ 1.0 (lower-bound y))
    )
  )
)

(define 
  (par1 r1 r2)
  (div-interval 
    (mul-interval r1 r2)
    (add-interval r1 r2)
  )
)
(define 
  (par2 r1 r2)
  (let 
    ((one (make-interval 1 1)))
    (div-interval 
      one
      (add-interval 
        (div-interval one r1)
        (div-interval one r2)
      )
    )
  )
)

; Lem が正しいことを示す。
(par1 (make-center-percent 6.8 10) (make-center-percent 4.7 5))
(par2 (make-center-percent 6.8 10) (make-center-percent 4.7 5))
; '(2.201031010873943 . 3.4873689182805854)
; '(2.581558809636278 . 2.97332259363673)
; 確かに違う結果が得られる

; また、ある区間Aについて次を得る
(define A (make-center-percent 100.0 1.0))

A
; '(99.0 . 101.0)
(div-interval A A)
; '(0.9801980198019802 . 1.0202020202020203)

; これは明らかに正しくない。式A/AにおけるAは同一であり、99〜101の任意の実数となる。
; そのため、A/Aは常に1になるが、(div-interval A A)は'(1.0 . 1.0)を返さない。
; つまり、intervalに関する四則演算は項の同一性を考慮できていないということである。
