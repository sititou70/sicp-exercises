#lang racket

; from: https://stackoverflow.com/questions/13998388/running-code-from-sicp-section-3-5-4-with-drracket
(define the-empty-stream '())
(define 
  (stream-null? stream)
  (null? stream)
)
(define-syntax 
  cons-stream
  (syntax-rules 
    ()
    ((cons-stream head tail) 
      (cons head (delay tail))
    )
  )
)
(define 
  (stream-car stream)
  (car stream)
)
(define 
  (stream-cdr stream)
  (force (cdr stream))
)

; utils
(define 
  (stream-map proc . argstreams)
  (if (stream-null? (car argstreams)) 
    the-empty-stream
    (cons-stream 
      (apply proc (map stream-car argstreams))
      (apply stream-map 
             (cons proc (map stream-cdr argstreams))
      )
    )
  )
)

(define (add-streams s1 s2) (stream-map + s1 s2))

(define 
  (scale-stream stream factor)
  (stream-map 
    (lambda (x) 
      (* x factor)
    )
    stream
  )
)

(define 
  (print-stream-nth s n)

  (define 
    (iter s cnt)

    (display "[")
    (display cnt)
    (display "] ")
    (displayln (stream-car s))

    (if (< cnt (- n 1)) 
      (iter (stream-cdr s) (+ cnt 1))
      'done
    )
  )

  (iter s 0)
)

; main
; 級数の積を以下のように分解して計算する。
; s1_0 * s2_0 (式a)
; + s1_0 * (s2_1 + s2_2 + s2_3 ... ) (式b)
; + s1_1 * s2_0
;   + s1_1 * (s2_1 + s2_2 + s2_3 ... )
;   + s1_2 * s2_0
;     + s1_2 * (s2_1 + s2_2 + s2_3 ... )
;     + ...

; この式を2行ごとに分けると、再帰的な構造があるとわかる

; s1_0 * s2_0 (式a)
; + s1_0 * (s2_1 + s2_2 + s2_3 ... ) (式b)

; + s1_1 * s2_0
;   + s1_1 * (s2_1 + s2_2 + s2_3 ... )

;   + s1_2 * s2_0
;     + s1_2 * (s2_1 + s2_2 + s2_3 ... )

;     + ...

(define 
  (mul-series s1 s2)
  (cons-stream 
    (* (stream-car s1) (stream-car s2)) ; (式a)の計算
    (add-streams 
      (mul-series (stream-cdr s1) s2) ; (式b)の計算
      (scale-stream (stream-cdr s2) (stream-car s1)) ; (式a)でも(式b)でもない残りの部分
    )
  )
)

; この手続きは、実際にはコーシー積を順に計算する。
; 参考1：https://ja.wikipedia.org/wiki/%E3%82%B3%E3%83%BC%E3%82%B7%E3%83%BC%E7%A9%8D
; 参考2：https://mathlandscape.com/cauchy-prod/

(define 
  (integrate-series s)
  (define 
    (iter s n)
    (cons-stream 
      (* (/ 1 n) (stream-car s))
      (iter (stream-cdr s) (+ n 1))
    )
  )
  (iter s 1)
)

(define 
  cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1))
)
(define sine-series (cons-stream 0 (integrate-series cosine-series)))

(define 
  s
  (add-streams 
    (mul-series sine-series sine-series)
    (mul-series cosine-series cosine-series)
  )
)
(print-stream-nth s 10)
; [0] 1
; [1] 0
; [2] 0
; [3] 0
; [4] 0
; [5] 0
; [6] 0
; [7] 0
; [8] 0
; [9] 0
; 'done
