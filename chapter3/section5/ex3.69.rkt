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
  (interleave s1 s2)
  (if (stream-null? s1) 
    s2
    (cons-stream 
      (stream-car s1)
      (interleave s2 (stream-cdr s1))
    )
  )
)

(define 
  (pairs s t)
  (cons-stream 
    (list (stream-car s) (stream-car t))
    (interleave 
      (stream-map 
        (lambda (x) (list (stream-car s) x))
        (stream-cdr t)
      )
      (pairs (stream-cdr s) (stream-cdr t))
    )
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
  (triples s t u)
  (let 
    ( ;
     (s-car (stream-car s))
     (pairs-stream (pairs t u))
    )

    (cons-stream 
      (cons s-car (stream-car pairs-stream))
      (interleave 
        (stream-map 
          (lambda (pair) 
            (cons s-car pair)
          )
          (stream-cdr pairs-stream)
        )
        (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))
      )
    )
  )
)

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(print-stream-nth (triples integers integers integers) 10)
; [0] (1 1 1)
; [1] (1 1 2)
; [2] (2 2 2)
; [3] (1 2 2)
; [4] (2 2 3)
; [5] (1 1 3)
; [6] (3 3 3)
; [7] (1 2 3)
; [8] (2 3 3)
; [9] (1 1 4)

(define (square x) (* x x))
(define 
  pythagorean-triple-stream
  (stream-filter 
    (lambda (triple) 
      (= 
        (+ (square (car triple)) (square (cadr triple)))
        (square (caddr triple))
      )
    )
    (triples integers integers integers)
  )
)

(print-stream-nth pythagorean-triple-stream 6)
; [0] (3 4 5)
; [1] (6 8 10)
; [2] (5 12 13)
; [3] (9 12 15)
; [4] (8 15 17)
; [5] (12 16 20)
