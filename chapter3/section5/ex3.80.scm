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
    (lambda (x) (* x factor))
    stream
  )
)

(define 
  (integral delayed-integrand initial-value dt)
  (define 
    int
    (cons-stream 
      initial-value
      (let 
        ((integrand (force delayed-integrand)))
        (add-streams (scale-stream integrand dt) int)
      )
    )
  )
  int
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
  (RLC R L C dt)
  (define 
    (vi iL0 vC0)

    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0 dt))
    (define dvC (scale-stream iL (/ -1 C)))
    (define 
      diL
      (add-streams 
        (scale-stream vC (/ 1 L))
        (scale-stream iL (- (/ R L)))
      )
    )

    (cons vC iL)
  )
  vi
)

(define R 1)
(define L 1)
(define C 0.2)
(define dt 0.1)
(define iL0 0)
(define vC0 10)
(define RLC1 ((RLC R L C dt) iL0 vC0))

(define 
  (plot times)

  (define 
    (iter n vC iL)

    (display "{\"t\":")
    (display (* n dt))
    (display ",\"label\":\"vC\"")
    (display ",\"value\":")
    (display (stream-car vC))
    (display "},")

    (display "{\"t\":")
    (display (* n dt))
    (display ",\"label\":\"iL\"")
    (display ",\"value\":")
    (display (stream-car iL))
    (display "}")
    (display 
      (if (= n times) 
        ""
        ","
      )
    )

    (if (= n times) 
      'done
      (iter (+ n 1) (stream-cdr vC) (stream-cdr iL))
    )
  )

  (display 
    "```vega-lite
{
  \"width\": 500,
  \"height\": 500,
  \"encoding\": {
    \"x\": {\"field\": \"t\", \"type\": \"quantitative\"},
    \"y\": {\"field\": \"value\", \"type\": \"quantitative\"},
    \"detail\": {\"field\": \"label\", \"type\": \"nominal\"},
    \"color\": {\"field\": \"label\", \"type\": \"nominal\"}
  },
  \"mark\": \"line\",
  \"data\": {\"values\": ["
  )
  (iter 0 (car RLC1) (cdr RLC1))
  (displayln 
    "]}
}
```"
  )
)
(displayln "20秒後までのvC、iLをプロットする。電流、電圧ともに減衰振動する。")
(plot (/ 20 dt))
