#lang racket/base

(define (make-interval a b) (cons a b))
(define (lower-bound a) (car a))
(define (upper-bound a) (cdr a))

(define 
  (orig-mul-interval x y)
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
  (mul-interval x y)

  (define u upper-bound)
  (define l lower-bound)

  ; ある区間の符号を調べて数値で返す
  ; (+, +): 0
  ; (-, +): 1
  ; (-, -): 2
  (define 
    (check-interval-sign-pattern x)
    (cond 
      ((and (>= (l x) 0) (>= (u x) 0)) 0)
      ((and (< (l x) 0) (>= (u x) 0)) 1)
      (else 2)
    )
  )

  (let 
    ((xp (check-interval-sign-pattern x)) 
      (yp (check-interval-sign-pattern y))
    )
    (cond 
      ((= xp 0)
       (cond 
         ((= yp 0) (make-interval (* (l x) (l y)) (* (u x) (u y))))
         ((= yp 1) (make-interval (* (u x) (l y)) (* (u x) (u y))))
         ((= yp 2) (make-interval (* (u x) (l y)) (* (l x) (u y))))
       )
      )
      ((= xp 1)
       (cond 
         ((= yp 0) (make-interval (* (l x) (u y)) (* (u x) (u y))))
         ((= yp 1)
          (make-interval 
            (min 
              (* (l x) (u y))
              (* (u x) (l y))
            )
            (max 
              (* (l x) (l y))
              (* (u x) (u y))
            )
          )
         )
         ((= yp 2) (make-interval (* (u x) (l y)) (* (l x) (l y))))
       )
      )
      ((= xp 2)
       (cond 
         ((= yp 0) (make-interval (* (l x) (u y)) (* (u x) (l y))))
         ((= yp 1) (make-interval (* (l x) (u y)) (* (l x) (l y))))
         ((= yp 2) (make-interval (* (u x) (u y)) (* (l x) (l y))))
       )
      )
    )
  )
)

; 新旧の手続きの結果が同じならtrueを返す
(define 
  (test x y)

  (define 
    (equal-interval? x y)
    (and 
      (= (lower-bound x) (lower-bound y))
      (= (upper-bound x) (upper-bound y))
    )
  )

  (equal-interval? (orig-mul-interval x y) (mul-interval x y))
)

; 以下はすべて#tを表示する
(test (make-interval 1 2) (make-interval 3 4))
(test (make-interval -1 2) (make-interval 3 4))
(test (make-interval -2 -1) (make-interval 3 4))

(test (make-interval 1 2) (make-interval -3 4))
(test (make-interval -1 2) (make-interval -3 4))
(test (make-interval -2 -1) (make-interval -3 4))

(test (make-interval 1 2) (make-interval -4 -3))
(test (make-interval -1 2) (make-interval -4 -3))
(test (make-interval -2 -1) (make-interval -4 -3))


(test (make-interval 3 4) (make-interval 1 2))
(test (make-interval 3 4) (make-interval -1 2))
(test (make-interval 3 4) (make-interval -2 -1))

(test (make-interval -3 4) (make-interval 1 2))
(test (make-interval -3 4) (make-interval -1 2))
(test (make-interval -3 4) (make-interval -2 -1))

(test (make-interval -4 -3) (make-interval 1 2))
(test (make-interval -4 -3) (make-interval -1 2))
(test (make-interval -4 -3) (make-interval -2 -1))


(test (make-interval 0 2) (make-interval 3 4))
(test (make-interval -1 0) (make-interval 3 4))
(test (make-interval 1 2) (make-interval 0 4))
(test (make-interval 1 2) (make-interval -3 0))
(test (make-interval -1 0) (make-interval -3 0))
(test (make-interval 0 2) (make-interval -3 0))
(test (make-interval 0 0) (make-interval -3 0))

(test (make-interval 3 4) (make-interval 0 2))
(test (make-interval 3 4) (make-interval -1 0))
(test (make-interval 0 4) (make-interval 1 2))
(test (make-interval -3 0) (make-interval 1 2))
(test (make-interval -3 0) (make-interval -1 0))
(test (make-interval -3 0) (make-interval 0 2))
(test (make-interval -3 0) (make-interval 0 0))
