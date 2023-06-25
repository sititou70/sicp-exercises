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
  (stream-map proc s)
  (if (stream-null? s) 
    the-empty-stream
    (cons-stream 
      (proc (stream-car s))
      (stream-map proc (stream-cdr s))
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
  (merge s1 s2)
  (cond 
    ((stream-null? s1) s2)
    ((stream-null? s2) s1)
    (else
     (let 
       ((s1car (stream-car s1)) 
         (s2car (stream-car s2))
       )
       (cond 
         ((< s1car s2car)
          (cons-stream 
            s1car
            (merge (stream-cdr s1) s2)
          )
         )
         ((> s1car s2car)
          (cons-stream 
            s2car
            (merge s1 (stream-cdr s2))
          )
         )
         (else
          (cons-stream 
            s1car
            (merge 
              (stream-cdr s1)
              (stream-cdr s2)
            )
          )
         )
       )
     )
    )
  )
)

(define 
  (scale-stream stream factor)
  (stream-map 
    (lambda (x) (* x factor))
    stream
  )
)

(define 
  S
  (cons-stream 
    1
    (merge 
      (scale-stream S 2)
      (merge 
        (scale-stream S 3)
        (scale-stream S 5)
      )
    )
  )
)

(print-stream-nth S 30)
; [0] 1
; [1] 2
; [2] 3
; [3] 4
; [4] 5
; [5] 6
; [6] 8
; [7] 9
; [8] 10
; [9] 12
; [10] 15
; [11] 16
; [12] 18
; [13] 20
; [14] 24
; [15] 25
; [16] 27
; [17] 30
; [18] 32
; [19] 36
; [20] 40
; [21] 45
; [22] 48
; [23] 50
; [24] 54
; [25] 60
; [26] 64
; [27] 72
; [28] 75
; [29] 80
