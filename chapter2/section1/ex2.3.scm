#lang racket/base

(define 
  (average x y)
  (/ (+ x y) 2)
)

(define 
  (make-point x y)
  (cons x y)
)
(define 
  (x-point p)
  (car p)
)
(define 
  (y-point p)
  (cdr p)
)

; 長方形の表現1。左下の点、幅、高さ、底辺とx軸が成す一般角による
(define 
  (make-rect1 left-down width height angle)
  (cons (cons left-down angle) (cons width height))
)
(define 
  (width-rect1 rect)
  (car (cdr rect))
)
(define 
  (height-rect1 rect)
  (cdr (cdr rect))
)

; 長方形の表現2。左下、左上、右下の3点による
(define 
  (make-rect2 left-down left-up right-down)
  (cons left-down (cons left-up right-down))
)
(define 
  (left-down-rect2 rect)
  (car rect)
)
(define 
  (left-up-rect2 rect)
  (car (cdr rect))
)
(define 
  (right-down-rect2 rect)
  (cdr (cdr rect))
)

(define 
  (length-points p1 p2)
  (sqrt 
    (+ 
      (expt 
        (- 
          (x-point p1)
          (x-point p2)
        )
        2
      )
      (expt 
        (- 
          (y-point p1)
          (y-point p2)
        )
        2
      )
    )
  )
)
(define 
  (width-rect2 rect)
  (length-points (left-down-rect2 rect) (right-down-rect2 rect))
)
(define 
  (height-rect2 rect)
  (length-points (left-down-rect2 rect) (left-up-rect2 rect))
)

; 円周および面積を求める手続き。どちらもrectの幅と高さの手続きにしか依存していない
(define 
  (perimeter-rect1 rect)
  (+ 
    (* (width-rect1 rect) 2)
    (* (height-rect1 rect) 2)
  )
)
(define 
  (perimeter-rect2 rect)
  (+ 
    (* (width-rect2 rect) 2)
    (* (height-rect2 rect) 2)
  )
)

(define 
  (area-rect1 rect)
  (* 
    (width-rect1 rect)
    (height-rect1 rect)
  )
)
(define 
  (area-rect2 rect)
  (* 
    (width-rect2 rect)
    (height-rect2 rect)
  )
)

; どちらも同じ長方形
(define pi 3.1415926535897932384626433832795)
(define 
  rect1
  (make-rect1 
    (make-point 1 0)
    (* 2 (sqrt 2))
    (sqrt 2)
    (/ pi 4)
  )
)
(define 
  rect2
  (make-rect2 
    (make-point 1 0)
    (make-point 0 1)
    (make-point 3 2)
  )
)

(perimeter-rect1 rect1)
; 8.485281374238571
(perimeter-rect2 rect2)
; 8.485281374238571

(area-rect1 rect1)
; 4.000000000000001
(area-rect2 rect2)
; 4.000000000000001
