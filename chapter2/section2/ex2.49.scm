#lang racket/base
(require 2htdp/image)
(require srfi/1)

; canvas
(define width 300)
(define height 300)
(define canvas null)

(define 
  (clear-canvas)
  (set! 
    canvas
    (rectangle width height "solid" "white")
  )
)

; vect
(define 
  (make-vect x y)
  (cons x y)
)
(define 
  (xcor-vect v)
  (car v)
)
(define 
  (ycor-vect v)
  (cdr v)
)

(define 
  (add-vect v1 v2)
  (make-vect 
    (+ (xcor-vect v1) (xcor-vect v2))
    (+ (ycor-vect v1) (ycor-vect v2))
  )
)
(define 
  (sub-vect v1 v2)
  (make-vect 
    (- (xcor-vect v1) (xcor-vect v2))
    (- (ycor-vect v1) (ycor-vect v2))
  )
)
(define 
  (scale-vect s v)
  (make-vect 
    (* s (xcor-vect v))
    (* s (ycor-vect v))
  )
)

; segment
(define 
  (make-segment start-vec end-vec)
  (cons start-vec end-vec)
)
(define 
  (start-segment seg)
  (car seg)
)
(define 
  (end-segment seg)
  (cdr seg)
)

; frame
(define 
  (make-frame origin edge1 edge2)
  (list origin edge1 edge2)
)
(define 
  (origin-frame frame)
  (car frame)
)
(define 
  (edge1-frame frame)
  (cadr frame)
)
(define 
  (edge2-frame frame)
  (caddr frame)
)

; utils for vect segment frame
(define 
  (frame-coord-map frame)
  (lambda (v) 
    (add-vect 
      (origin-frame frame)
      (add-vect 
        (scale-vect (xcor-vect v) (edge1-frame frame))
        (scale-vect (ycor-vect v) (edge2-frame frame))
      )
    )
  )
)

; drawing utils
(define 
  (draw-line v1 v2)
  (set! 
    canvas
    (add-line 
      canvas
      (xcor-vect v1)
      (ycor-vect v1)
      (xcor-vect v2)
      (ycor-vect v2)
      "black"
    )
  )
)

(define 
  (segments->painter segment-list)
  (lambda (frame) 
    (for-each 
      (lambda (segment) 
        (draw-line 
          ((frame-coord-map frame) 
            (start-segment segment)
          )
          ((frame-coord-map frame) 
            (end-segment segment)
          )
        )
      )
      segment-list
    )
  )
)

; 3次ベジェ曲線をセグメントの配列に変換する
; bezierは以下のような構造
;  (list 
;    [初期点]
;    (list [前の点と制御点の差] [現在の点と制御点の差] [前の点と現在の点の差])
;    (list [前の点と制御点の差] [現在の点と制御点の差] [前の点と現在の点の差])
;    (list [前の点と制御点の差] [現在の点と制御点の差] [前の点と現在の点の差])
;    ...
;  )
; 特に、以下の構造をcと呼ぶ
;    (list [前の点と制御点の差] [現在の点と制御点の差] [前の点と現在の点の差])
(define 
  (bezier->segments bezier)

  (define resolution 20)

  (define 
    (render prev d-prevc d-currentc d-current)

    (define 
      (bezier-point time)

      ; nが3のときのBernstein基底関数
      ; https://ja.wikipedia.org/wiki/%E3%83%90%E3%83%BC%E3%83%B3%E3%82%B9%E3%82%BF%E3%82%A4%E3%83%B3%E5%A4%9A%E9%A0%85%E5%BC%8F
      (define 
        (bernstein v)
        (let 
          ((binomial-coefficients 
             (cond 
               ((= v 0) 1)
               ((= v 1) 3)
               ((= v 2) 3)
               ((= v 3) 1)
             )
           ) 
          )
          (* binomial-coefficients (expt time v) (expt (- 1 time) (- 3 v)))
        )
      )

      ; 参考：https://ja.wikipedia.org/wiki/%E3%83%99%E3%82%B8%E3%82%A7%E6%9B%B2%E7%B7%9A
      (let 
        ((p0 prev) 
          (p1 (add-vect prev d-prevc))
          (p2 (add-vect prev d-currentc))
          (p3 (add-vect prev d-current))
        )
        (add-vect 
          (scale-vect (bernstein 0) p0)
          (add-vect 
            (scale-vect (bernstein 1) p1)
            (add-vect 
              (scale-vect (bernstein 2) p2)
              (scale-vect (bernstein 3) p3)
            )
          )
        )
      )
    )

    (map 
      (lambda (index) 
        (let 
          ((current-time (* (/ 1.0 resolution) index)) 
            (next-time (* (/ 1.0 resolution) (+ index 1)))
          )
          (make-segment (bezier-point current-time) (bezier-point next-time))
        )
      )
      (iota resolution)
    )
  )

  (define 
    (iter prev c-list segments)
    (if (null? c-list) 
      segments
      (let 
        ((rest-c-list (cdr c-list)) 
          (d-prevc (caar c-list))
          (d-currentc (cadar c-list))
          (d-current (caddar c-list))
        )
        (iter 
          (add-vect prev d-current)
          rest-c-list
          (append segments (render prev d-prevc d-currentc d-current))
        )
      )
    )
  )

  (iter (car bezier) (cdr bezier) null)
)

; painters
(define 
  frame-painter
  (segments->painter 
    (list 
      (make-segment 
        (make-vect 0 0)
        (make-vect 1 0)
      )
      (make-segment 
        (make-vect 0 0)
        (make-vect 0 1)
      )
      (make-segment 
        (make-vect 0 1)
        (make-vect 1 1)
      )
      (make-segment 
        (make-vect 1 0)
        (make-vect 1 1)
      )
    )
  )
)
(define 
  x-painter
  (segments->painter 
    (list 
      (make-segment 
        (make-vect 0 0)
        (make-vect 1 1)
      )
      (make-segment 
        (make-vect 1 0)
        (make-vect 0 1)
      )
    )
  )
)
(define 
  diamond-painter
  (segments->painter 
    (list 
      (make-segment 
        (make-vect 0.5 0)
        (make-vect 0 0.5)
      )
      (make-segment 
        (make-vect 0.5 0)
        (make-vect 1 0.5)
      )
      (make-segment 
        (make-vect 0 0.5)
        (make-vect 0.5 1)
      )
      (make-segment 
        (make-vect 1 0.5)
        (make-vect 0.5 1)
      )
    )
  )
)
; inkscapeでトレースした結果のベジェ曲線を描画している
(define 
  wave-painter
  (segments->painter 
    (append 
      (bezier->segments 
        (list 
          (make-vect 0 0.27)
          (list (make-vect 0 0) (make-vect 0.13 0.21) (make-vect 0.22 0.1))
          (list (make-vect 0.08 -0.09) 
                (make-vect 0.13 -0.05)
                (make-vect 0.15 -0.05)
          )
          (list (make-vect 0.05 0.02) (make-vect 0.06 -0.05) (make-vect 0 -0.09))
          (list (make-vect -0.09 -0.05) 
                (make-vect 0.01 -0.2)
                (make-vect 0.04 -0.24)
          )
        )
      )
      (bezier->segments 
        (list 
          (make-vect 0 0.38)
          (list (make-vect 0 0) (make-vect 0.1 0.24) (make-vect 0.22 0.1))
          (list (make-vect 0.05 -0.05) 
                (make-vect 0.26 -0.18)
                (make-vect 0.02 0.52)
          )
        )
      )
      (bezier->segments 
        (list 
          (make-vect 0.37 1)
          (list (make-vect 0 0) (make-vect 0.1 -0.67) (make-vect 0.27 0))
        )
      )
      (bezier->segments 
        (list 
          (make-vect 0.76 1)
          (list (make-vect 0 0) (make-vect -0.47 -1.02) (make-vect 0.23 -0.27))
        )
      )
      (bezier->segments 
        (list 
          (make-vect 1 0.62)
          (list (make-vect 0 0) (make-vect -0.25 -0.34) (make-vect -0.37 -0.29))
          (list (make-vect -0.12 0.04) 
                (make-vect -0.05 -0.07)
                (make-vect -0.03 -0.09)
          )
          (list (make-vect 0.08 -0.08) 
                (make-vect -0.02 -0.19)
                (make-vect -0.05 -0.23)
          )
        )
      )
    )
  )
)

(define 
  canvas-frame
  (make-frame (make-vect 0 0) (make-vect width 0) (make-vect 0 height))
)

(clear-canvas)
(frame-painter canvas-frame)
(save-image canvas "ex2.49.a.png")

(clear-canvas)
(x-painter canvas-frame)
(save-image canvas "ex2.49.b.png")

(clear-canvas)
(diamond-painter canvas-frame)
(save-image canvas "ex2.49.c.png")

(clear-canvas)
(wave-painter canvas-frame)
(save-image canvas "ex2.49.d.png")
