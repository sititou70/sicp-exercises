#lang racket/base

(require "data-directed-utils.scm")

; --------
; 複素数：直交形式パッケージ
; --------

(define 
  (install-rectangular-package)

  ;; 内部手続き
  (define (square x) (* x x))

  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define 
    (magnitude z)
    (sqrt 
      (+ (square (real-part z)) 
         (square (imag-part z))
      )
    )
  )
  (define 
    (angle z)
    (atan (imag-part z) (real-part z))
  )
  (define 
    (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a)))
  )

  ;; システムのほかの部分とのインターフェイス
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 
    'make-from-real-imag
    'rectangular
    (lambda (x y) (tag (make-from-real-imag x y)))
  )
  (put 
    'make-from-mag-ang
    'rectangular
    (lambda (r a) (tag (make-from-mag-ang r a)))
  )

  'done
)
(install-rectangular-package)
