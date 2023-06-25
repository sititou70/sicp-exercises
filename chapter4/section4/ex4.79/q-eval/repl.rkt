#lang racket
(provide (all-defined-out))

(require sicp)
(require "stream.rkt")
(require "expressions.rkt")
(require "environment.rkt")
(require "eval-apply.rkt")

(define 
  (make-repl)

  (define the-global-world (new-empty-world))

  (define 
    (repl input)

    (let 
      ( ;
       (q (query-syntax-process input))
      )

      (cond 
        ((assertion-to-be-added? q)
         (let 
           ( ;
            (result-stream 
              (qeval q (singleton-stream the-global-world))
            )
           )

           (set! the-global-world (internal-stream-car result-stream))
         )
        )
        (else
         (let 
           ( ;
            (result-stream 
              (stream-map 
                (lambda (world) 
                  (instantiate 
                    (remove-assert 
                      (set-env-id-to-variables-in 
                        q
                        (world-current-environment-id world)
                      )
                    )
                    world
                    (lambda (v w) 
                      (contract-question-mark v)
                    )
                  )
                )
                (qeval q (singleton-stream the-global-world))
              )
            )
           )

           (display-stream result-stream)
         )
        )
      )
    )
  )

  repl
)

; クエリの表現を内部表現に変換
(define 
  (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp)
)
(define 
  (map-over-symbols proc exp)
  (cond 
    ((pair? exp)
     (cons 
       (map-over-symbols proc (car exp))
       (map-over-symbols proc (cdr exp))
     )
    )
    ((symbol? exp) (proc exp))
    (else exp)
  )
)
(define 
  (expand-question-mark symbol)
  (let 
    ( ;
     (chars (symbol->string symbol))
    )

    (if (string=? (substring chars 0 1) "?") 
      (make-variable 
        '() ; クエリは必ずトップレベルの環境で評価されるため
        (string->symbol 
          (substring chars 1 (string-length chars))
        )
      )
      symbol
    )
  )
)

; 内部表現を表示用の表現に変換
(define 
  (contract-question-mark variable)
  (string->symbol 
    (string-append 
      "?"
      (if (number? (cadr variable)) 
        (string-append 
          (symbol->string (caddr variable))
          "-"
          (number->string (cadr variable))
        )
        (symbol->string (cadr variable))
      )
    )
  )
)

; assert!を削除する
(define 
  (remove-assert exp)

  (define 
    (filter predicate sequence)
    (cond 
      ((null? sequence) '())
      ((predicate (car sequence))
       (cons 
         (car sequence)
         (filter predicate (cdr sequence))
       )
      )
      (else (filter predicate (cdr sequence)))
    )
  )

  (cond 
    ((pair? exp)
     (filter (lambda (exp) (not (and (pair? exp) (eq? (car exp) 'assert!)))) exp)
    )
    (else exp)
  )
)
