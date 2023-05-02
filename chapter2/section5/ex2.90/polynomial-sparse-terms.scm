#lang racket/base

(require "data-directed-utils.scm")
(require "generic-procs.scm")

; --------
; 多項式パッケージ：疎な項リスト
; --------

(define 
  (install-sparse-terms)

  ;; 内部手続き
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))

  (define 
    (adjoin-term term-list)
    (lambda (order coeff) 
      (if (=zero? coeff) 
        term-list
        (cons (list order coeff) term-list)
      )
    )
  )

  ;; システムのほかの部分とのインターフェイス
  (define (tag terms-list) (attach-tag 'sparse-terms terms-list))

  (put 
    'first-term
    '(sparse-terms)
    (lambda (terms-list) (first-term terms-list))
  )
  (put 
    'rest-terms
    '(sparse-terms)
    (lambda (terms-list) (tag (rest-terms terms-list)))
  )
  (put 
    'adjoin-term
    '(sparse-terms)
    (lambda (terms-list) 
      (let 
        ((proc (adjoin-term terms-list)))
        (lambda (order coeff) (tag (proc order coeff)))
      )
    )
  )
  (put 
    'make
    'sparse-terms
    (lambda (terms-list) (tag terms-list))
  )

  'done
)
(install-sparse-terms)
