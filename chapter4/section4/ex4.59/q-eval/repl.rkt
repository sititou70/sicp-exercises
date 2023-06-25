#lang racket
(provide (all-defined-out))

(require sicp)
(require "stream.rkt")
(require "expressions.rkt")
(require "database.rkt")
(require "eval-apply.rkt")

(define 
  (make-repl mode)

  (define 
    (repl input)

    (let 
      ( ;
       (q (query-syntax-process input))
      )

      (cond 
        ((assertion-to-be-added? q)
         (add-rule-or-assertion! (add-assertion-body q))
         (if (eq? mode 'display) (displayln "Assertion added to data base."))
        )
        (else
         (let 
           ( ;
            (result-stream 
              (stream-map 
                (lambda (frame) 
                  (instantiate 
                    q
                    frame
                    (lambda (v f) 
                      (contract-question-mark v)
                    )
                  )
                )
                (qeval q (singleton-stream '()))
              )
            )
           )

           (if (eq? mode 'display) (display-stream result-stream))
         )
        )
      )
    )
  )

  repl
)

; transform query -> internal variable representation
(define 
  (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp)
)
(define 
  (map-over-symbols proc exp)
  (cond 
    ((pair? exp)
     (cons (map-over-symbols proc (car exp)) 
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
      (list '? 
            (string->symbol 
              (substring chars 1 (string-length chars))
            )
      )
      symbol
    )
  )
)

; transform internal variable representation -> human-readable symbol
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
