#lang racket/base
(require 2htdp/image)
(require srfi/1)

; canvas
(define width 800)
(define height 800)
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

; transformers
(define 
  (transform-painter painter origin corner1 corner2)
  (lambda (frame) 
    (let 
      ((m (frame-coord-map frame)))
      (let 
        ((new-origin (m origin)))
        (painter 
          (make-frame 
            new-origin
            (sub-vect (m corner1) new-origin)
            (sub-vect (m corner2) new-origin)
          )
        )
      )
    )
  )
)

(define 
  (identity painter)
  (transform-painter 
    painter
    (make-vect 0.0 0.0)
    (make-vect 1.0 0.0)
    (make-vect 0.0 1.0)
  )
)

(define 
  (rotate90 painter)
  (transform-painter 
    painter
    (make-vect 1.0 0.0)
    (make-vect 1.0 1.0)
    (make-vect 0.0 0.0)
  )
)
(define 
  (rotate180 painter)
  (transform-painter 
    painter
    (make-vect 1.0 1.0)
    (make-vect 0.0 1.0)
    (make-vect 1.0 0.0)
  )
)
(define 
  (rotate270 painter)
  (transform-painter 
    painter
    (make-vect 0.0 1.0)
    (make-vect 0.0 0.0)
    (make-vect 1.0 1.0)
  )
)

(define 
  (flip-vert painter)
  (transform-painter 
    painter
    (make-vect 0.0 1.0)
    (make-vect 1.0 1.0)
    (make-vect 0.0 0.0)
  )
)
(define 
  (flip-horiz painter)
  (transform-painter 
    painter
    (make-vect 1.0 0.0)
    (make-vect 0.0 0.0)
    (make-vect 1.0 1.0)
  )
)

(define 
  (beside painter1 painter2)
  (let 
    ((split-point (make-vect 0.5 0.0)))
    (let 
      ((paint-left 
         (transform-painter 
           painter1
           (make-vect 0.0 0.0)
           split-point
           (make-vect 0.0 1.0)
         )
       ) 
        (paint-right 
          (transform-painter 
            painter2
            split-point
            (make-vect 1.0 0.0)
            (make-vect 0.5 1.0)
          )
        )
      )
      (lambda (frame) 
        (paint-left frame)
        (paint-right frame)
      )
    )
  )
)

(define 
  (below painter1 painter2)
  (let 
    ((split-point (make-vect 0.0 0.5)))
    (let 
      ((paint-left 
         (transform-painter 
           painter1
           (make-vect 0.0 0.0)
           (make-vect 1.0 0.0)
           split-point
         )
       ) 
        (paint-right 
          (transform-painter 
            painter2
            split-point
            (make-vect 1.0 0.5)
            (make-vect 0.0 1.0)
          )
        )
      )
      (lambda (frame) 
        (paint-left frame)
        (paint-right frame)
      )
    )
  )
)

; painters
; inkscapeでトレースしたwave.svgのベジェ曲線を描画している
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
      ; 服を追加する
      (bezier->segments 
        (list (make-vect 0.37 0.33) 
              (list (make-vect 0 0) (make-vect 0.09 0.14) (make-vect 0.22 0.01))
        )
      )
      (bezier->segments 
        (list (make-vect 0.19 0.40) 
              (list (make-vect 0 0) (make-vect 0 0.04) (make-vect 0.04 0.07))
        )
      )
      (bezier->segments 
        (list (make-vect 0.77 0.38) 
              (list (make-vect 0 0) (make-vect 0.01 0.06) (make-vect -0.06 0.09))
        )
      )
      (bezier->segments 
        (list (make-vect 0.29 0.84) 
              (list (make-vect 0 0) (make-vect 0.03 0.05) (make-vect 0.1 0.03))
        )
      )
      (bezier->segments 
        (list (make-vect 0.60 0.86) 
              (list (make-vect 0 0) (make-vect 0.05 0.01) (make-vect 0.08 -0.03))
        )
      )
    )
  )
)

; patterns
(define 
  (split first-split second-split)
  (lambda (painter n) 
    (if (= n 0) 
      painter
      (let 
        ((smaller 
           ((split first-split second-split) 
             painter
             (- n 1)
           )
         ) 
        )
        (first-split painter (second-split smaller smaller))
      )
    )
  )
)
(define right-split (split beside below))
(define bottom-split (split below beside))
; 座標系が違うため右下に向かってsplitしていく
(define 
  (corner-split painter n)
  (if (= n 0) 
    painter
    (let 
      ((down (bottom-split painter (- n 1))) 
        (right (right-split painter (- n 1)))
      )
      (let 
        ((bottom-left (beside down down)) 
          (up-right (below right right))
          (corner (corner-split painter (- n 1)))
        )
        (beside 
          (below painter bottom-left)
          (below up-right corner)
        )
      )
    )
  )
)
(define right-split2 (split beside (lambda (p1 p2) (rotate90 (below p1 p2)))))
(define bottom-split2 (split below (lambda (p1 p2) (rotate270 (beside p1 p2)))))
(define 
  (corner-split2 painter n)
  (if (= n 0) 
    painter
    (let 
      ((down (bottom-split2 painter (- n 1))) 
        (right (right-split2 painter (- n 1)))
      )
      (let 
        ((bottom-left (beside down down)) 
          (up-right (below right right))
          (corner (corner-split2 painter (- n 1)))
        )
        (beside 
          (below painter (rotate90 bottom-left))
          (below (rotate270 up-right) (rotate180 corner))
        )
      )
    )
  )
)
(define 
  (square-of-four tl tr bl br)
  (lambda (painter) 
    (let 
      ((top (beside (tl painter) (tr painter))) 
        (bottom (beside (bl painter) (br painter)))
      )
      (below bottom top)
    )
  )
)
(define 
  (square-limit painter n)
  (let 
    ((combine4 
       (square-of-four 
         flip-horiz
         identity
         rotate180
         flip-vert
       )
     ) 
    )
    (combine4 (corner-split painter n))
  )
)
(define 
  (square-limit2 painter n)
  (let 
    ((combine4 
       (square-of-four 
         flip-vert
         rotate180
         identity
         flip-horiz
       )
     ) 
    )
    (combine4 (corner-split painter n))
  )
)

(define 
  canvas-frame
  (make-frame (make-vect 0 0) (make-vect width 0) (make-vect 0 height))
)

(clear-canvas)
((corner-split wave-painter 5) canvas-frame)
(save-image canvas "ex2.52.corner-split.png")

(clear-canvas)
((corner-split2 wave-painter 5) canvas-frame)
(save-image canvas "ex2.52.corner-split2.png")

(clear-canvas)
((square-limit wave-painter 4) canvas-frame)
(save-image canvas "ex2.52.square-limit.png")

(clear-canvas)
((square-limit2 wave-painter 4) canvas-frame)
(save-image canvas "ex2.52.square-limit2.png")
