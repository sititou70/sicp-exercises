#lang racket/base

; see: http://community.schemewiki.org/?sicp-ex-2.73
(define *procedures* (make-hash))
(define (put key1 key2 value) (hash-set! *procedures* (list key1 key2) value))
(define (get key1 key2) (hash-ref *procedures* (list key1 key2) #f))

; 日本オフィスのコード
(define 
  jp-db
  '(((name tanaka) (address tokyo) (salary 100))
    ((name sato) (address osaka) (salary 200))
   )
)

(define 
  (install-jp-utils)

  (define 
    (get-record name)
    (cons 
      'jp
      (findf 
        (lambda (item) (eq? (cadar item) name))
        jp-db
      )
    )
  )
  (put 'jp 'get-record get-record)

  (define (get-salary rec) (cadr (caddr rec)))
  (put 'jp 'get-salary get-salary)
)

; アメリカオフィスのコード
(define 
  us-db
  '((alice 200 la)
    (bob 400 ny)
   )
)

(define 
  (install-us-utils)

  (define 
    (get-record name)
    (cons 
      'us
      (findf 
        (lambda (item) (eq? (car item) name))
        us-db
      )
    )
  )
  (put 'us 'get-record get-record)

  (define (get-salary rec) (cadr rec))
  (put 'us 'get-salary get-salary)
)

; 本部のコード
(define 
  (get-salary rec)

  ((get (car rec) 'get-salary) 
    (cdr rec)
  )
)

(define 
  (find-employee-record name office-list)

  (findf 
    (lambda (item) (pair? (cdr item)))
    (map 
      (lambda (office) 
        ((get office 'get-record) name)
      )
      office-list
    )
  )
)

(install-jp-utils)
(install-us-utils)

; a.
((get 'jp 'get-record) 'tanaka)
((get 'us 'get-record) 'bob)
; '(jp (name tanaka) (address tokyo) (salary 100))
; '(us bob 400 ny)

; b.
(get-salary ((get 'jp 'get-record) 'tanaka))
; 100
(get-salary ((get 'us 'get-record) 'bob))
; 400

; c.
(find-employee-record 'sato '(jp us))
(find-employee-record 'alice '(jp us))

; d. 今後オフィスが増えた場合でも、オフィスごとのコードを増やすだけで良い。本部のコードを変更する必要はない。
