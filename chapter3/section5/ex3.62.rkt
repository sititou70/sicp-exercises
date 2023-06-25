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
(define 
  (mul-series s1 s2)
  (cons-stream 
    (* (stream-car s1) (stream-car s2))
    (add-streams 
      (mul-series (stream-cdr s1) s2)
      (scale-stream (stream-cdr s2) (stream-car s1))
    )
  )
)

(define 
  (invert-unit-series s)
  (cons-stream 
    1
    (scale-stream 
      (mul-series 
        (stream-cdr s)
        (invert-unit-series s)
      )
      -1
    )
  )
)

(define 
  (div-series s1 s2)
  (if (= (stream-car s2) 0) 
    (error "The constant term of s2 is 0.")
    (mul-series 
      s1
      (invert-unit-series s2)
    )
  )
)

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

(define tangent-series (div-series sine-series cosine-series))
(print-stream-nth tangent-series 30)
; [0] 0
; [1] 1
; [2] 0
; [3] 1/3
; [4] 0
; [5] 2/15
; [6] 0
; [7] 17/315
; [8] 0
; [9] 62/2835
; [10] 0
; [11] 1382/155925
; [12] 0
; [13] 21844/6081075
; [14] 0
; [15] 929569/638512875
; [16] 0
; [17] 6404582/10854718875
; [18] 0
; [19] 443861162/1856156927625
; [20] 0
; [21] 18888466084/194896477400625
; [22] 0
; [23] 113927491862/2900518163668125
; [24] 0
; [25] 58870668456604/3698160658676859375
; [26] 0
; [27] 8374643517010684/1298054391195577640625
; [28] 0
; [29] 689005380505609448/263505041412702261046875

; tan(pi / 3)を計算してsqrt(3)を近似してみる
(define 
  (tan x n)
  (define orig-x x)
  (define 
    (iter x n s sum)
    (if (= n 0) 
      sum
      (iter 
        (* x orig-x)
        (- n 1)
        (stream-cdr s)
        (+ sum (* (stream-car s) x))
      )
    )
  )
  (iter 1 n tangent-series 0)
)
(define pi 3.1415926535897932384626433832795)
(tan (/ pi 3) 100)
; 1.7320508075688763
; 1.73205080756887729352... ; 真の値
