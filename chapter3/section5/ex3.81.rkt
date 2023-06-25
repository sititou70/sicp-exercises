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
  (list2stream list)
  (if (null? list) 
    the-empty-stream
    (cons-stream 
      (car list)
      (list2stream (cdr list))
    )
  )
)

(define 
  commands
  (list2stream 
    '((reset 123)
      (generate)
      (generate)
      (generate)
      (generate)
      (reset 123)
      (generate)
      (generate)
      (generate)
      (generate)
     )
  )
)

(define 
  (rendom-numbers commands)

  (define a 4086618881)
  (define b 6945494267)
  (define m 8321946851)
  (define 
    (update-random prev-random)
    (remainder (+ (* a prev-random) b) m)
  )

  (cons-stream 
    0
    (stream-map 
      (lambda (c prev-random) 
        (cond 
          ((eq? (car c) 'generate)
           (update-random prev-random)
          )
          ((eq? (car c) 'reset)
           (cadr c)
          )
          (else (error "Unknown Command!"))
        )
      )
      commands
      (rendom-numbers commands)
    )
  )
)

(print-stream-nth (rendom-numbers commands) 11)
; [0] 0
; [1] 123
; [2] 1960858719
; [3] 7125103636
; [4] 4544539760
; [5] 7494328840
; [6] 123
; [7] 1960858719
; [8] 7125103636
; [9] 4544539760
; [10] 7494328840
