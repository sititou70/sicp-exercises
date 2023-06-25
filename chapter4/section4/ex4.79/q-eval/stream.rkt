#lang racket
(provide 
  (except-out 
    (all-defined-out)
    the-empty-stream
    stream-null?
    cons-stream
    stream-car
    stream-cdr
  )
)

(require sicp)

; primitive procedures
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
;; lang sicpと手続き名がかぶらないようにリネームしてprovideする
(define (internal-stream-cons x y) (cons-stream x y))
(define internal-stream-car stream-car)
(define internal-stream-cdr stream-cdr)
(define internal-the-empty-stream the-empty-stream)
(define internal-stream-null? stream-null?)

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
  (stream-for-each proc s)
  (if (stream-null? s) 
    'done
    (begin 
      (proc (stream-car s))
      (stream-for-each proc (stream-cdr s))
    )
  )
)

(define 
  (stream-append s1 s2)
  (if (stream-null? s1) 
    s2
    (cons-stream 
      (stream-car s1)
      (stream-append (stream-cdr s1) s2)
    )
  )
)

(define 
  (display-stream s)
  (stream-for-each display-line s)
)
(define (display-line x) (displayln x))

(define 
  (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1) 
    (force delayed-s2)
    (cons-stream 
      (stream-car s1)
      (stream-append-delayed 
        (stream-cdr s1)
        delayed-s2
      )
    )
  )
)

(define 
  (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1) 
    (force delayed-s2)
    (cons-stream 
      (stream-car s1)
      (interleave-delayed 
        (force delayed-s2)
        (delay (stream-cdr s1))
      )
    )
  )
)

(define 
  (stream-flatmap proc s)
  (flatten-stream (stream-map proc s))
)
(define 
  (flatten-stream stream)
  (if (stream-null? stream) 
    the-empty-stream
    (interleave-delayed 
      (stream-car stream)
      (delay (flatten-stream (stream-cdr stream)))
    )
  )
)

(define 
  (singleton-stream x)
  (cons-stream x the-empty-stream)
)
