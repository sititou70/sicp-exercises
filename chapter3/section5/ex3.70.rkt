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

(define 
  (stream-filter pred stream)
  (cond 
    ((stream-null? stream) the-empty-stream)
    ((pred (stream-car stream))
     (cons-stream 
       (stream-car stream)
       (stream-filter 
         pred
         (stream-cdr stream)
       )
     )
    )
    (else (stream-filter pred (stream-cdr stream)))
  )
)

(define (add-streams s1 s2) (stream-map + s1 s2))

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
  (merge-weighted s1 s2 weight)
  (cond 
    ((stream-null? s1) s2)
    ((stream-null? s2) s1)
    (else
     (let 
       ( ;
        (s1car (stream-car s1))
        (s2car (stream-car s2))
        (weight-s1 (apply weight (stream-car s1)))
        (weight-s2 (apply weight (stream-car s2)))
       )
       (cond 
         ((> weight-s1 weight-s2)
          (cons-stream 
            s2car
            (merge-weighted s1 (stream-cdr s2) weight)
          )
         )
         (else
          (cons-stream 
            s1car
            (merge-weighted (stream-cdr s1) s2 weight)
          )
         )
       )
     )
    )
  )
)

(define 
  (weighted-pairs s t weight)
  (cons-stream 
    (list (stream-car s) (stream-car t))
    (merge-weighted 
      (stream-map 
        (lambda (x) (list (stream-car s) x))
        (stream-cdr t)
      )
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
      weight
    )
  )
)

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

; a. 和 i + j の順に並んだ、すべての正の整数のペア (i; j) i <= jのストリーム
(print-stream-nth 
  (weighted-pairs 
    integers
    integers
    (lambda (i j) (+ i j))
  )
  10
)

; i と j のどちらも 2, 3, 5 で割り切れないすべての正の整数のペア (i; j) i <= j が 2i + 3j + 5ij の順に並んでいるストリーム
(define 
  filterd-integers
  (stream-filter 
    (lambda (x) 
      (not 
        (or 
          (= (remainder x 2) 0)
          (= (remainder x 3) 0)
          (= (remainder x 5) 0)
        )
      )
    )
    integers
  )
)
(print-stream-nth 
  (weighted-pairs 
    filterd-integers
    filterd-integers
    (lambda (i j) (+ (* 2 i) (* 3 j) (* 5 i j)))
  )
  30
)
; [0] (1 1)
; [1] (1 7)
; [2] (1 11)
; [3] (1 13)
; [4] (1 17)
; [5] (1 19)
; [6] (1 23)
; [7] (1 29)
; [8] (1 31)
; [9] (7 7)
; [10] (1 37)
; [11] (1 41)
; [12] (1 43)
; [13] (1 47)
; [14] (1 49)
; [15] (1 53)
; [16] (7 11)
; [17] (1 59)
; [18] (1 61)
; [19] (7 13)
; [20] (1 67)
; [21] (1 71)
; [22] (1 73)
; [23] (1 77)
; [24] (1 79)
; [25] (7 17)
; [26] (11 11)
; [27] (1 83)
; [28] (1 89)
; [29] (1 91)
