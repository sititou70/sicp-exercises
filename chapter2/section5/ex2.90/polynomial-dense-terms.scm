#lang racket/base

(require "data-directed-utils.scm")
(require "generic-procs.scm")

; --------
; 多項式パッケージ：密な項リスト
; --------

; タワーの中で最も低い型で数値を作成するコンストラクタ
; 新しい型を追加する場合は合わせて変更する
(define (make-bottom-number x) (make-scheme-number x))

(define 
  (install-dense-terms)

  ;; 内部手続き
  (define 
    (first-term term-list)
    (list 
      (length (cdr term-list)) ; order
      (car term-list) ; coeff
    )
  )
  (define (rest-terms term-list) (cdr term-list))

  (define 
    (adjoin-term term-list)
    (lambda (order coeff) 
      (cond 
        ((=zero? coeff)
         term-list
        )
        ((= order (length term-list))
         (cons coeff term-list)
        )
        (else
         ((adjoin-term 
            (cons (make-bottom-number 0) term-list)
          ) 
           order
           coeff
         )
        )
      )
    )
  )

  ;; システムのほかの部分とのインターフェイス
  (define (tag terms-list) (attach-tag 'dense-terms terms-list))

  (put 
    'first-term
    '(dense-terms)
    (lambda (terms-list) (first-term terms-list))
  )
  (put 
    'rest-terms
    '(dense-terms)
    (lambda (terms-list) (tag (rest-terms terms-list)))
  )
  (put 
    'adjoin-term
    '(dense-terms)
    (lambda (terms-list) 
      (let 
        ((proc (adjoin-term terms-list)))
        (lambda (order coeff) (tag (proc order coeff)))
      )
    )
  )
  (put 
    'make
    'dense-terms
    (lambda (terms-list) (tag terms-list))
  )

  'done
)
(install-dense-terms)
