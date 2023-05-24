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
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

; Louisのpairsは停止しない。これは、interleaveが無限に再帰的に評価されるからである
; オリジナルのpairsであれば、cons-streamの遅延評価によって評価は停止していた
(define 
  (louis-pairs s t)
  (interleave 
    (stream-map 
      (lambda (x) 
        (list (stream-car s) x)
      )
      t
    )
    (louis-pairs (stream-cdr s) (stream-cdr t))
  )
)
(stream-car (louis-pairs integers integers))
; 停止しない
