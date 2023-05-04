#lang racket/base
(provide (all-defined-out))

(require "data-directed-utils.scm")
(require "generic-procs.scm")
(require "complex-rectangular.scm")
(require "complex-polar.scm")

; --------
; 複素数算術演算パッケージ
; --------

(define 
  (install-complex-package)

  ;; 直交形式パッケージと極形式パッケージからインポートした手続き
  (define 
    (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y)
  )
  (define 
    (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a)
  )

  ;; 内部手続き
  (define 
    (add-complex z1 z2)
    (make-from-real-imag 
      (+ (real-part z1) (real-part z2))
      (+ (imag-part z1) (imag-part z2))
    )
  )
  (define 
    (sub-complex z1 z2)
    (make-from-real-imag 
      (- (real-part z1) (real-part z2))
      (- (imag-part z1) (imag-part z2))
    )
  )
  (define 
    (mul-complex z1 z2)
    (make-from-mag-ang 
      (* (magnitude z1) (magnitude z2))
      (+ (angle z1) (angle z2))
    )
  )
  (define 
    (div-complex z1 z2)
    (make-from-mag-ang 
      (/ (magnitude z1) (magnitude z2))
      (- (angle z1) (angle z2))
    )
  )
  (define 
    (equ?-complex z1 z2)
    (and 
      (= (magnitude z1) (magnitude z2))
      (= (angle z1) (angle z2))
    )
  )
  (define 
    (=zero?-complex z)
    (= 
      (magnitude z)
      0
    )
  )
  (define 
    (additive-inverse-complex z)
    (mul (tag z) (tag (make-from-mag-ang -1 0)))
  )

  ;; システムのほかの部分とのインターフェイス
  (define (tag z) (attach-tag 'complex z))
  (put 
    'add
    '(complex complex)
    (lambda (z1 z2) (tag (add-complex z1 z2)))
  )
  (put 
    'sub
    '(complex complex)
    (lambda (z1 z2) (tag (sub-complex z1 z2)))
  )
  (put 
    'mul
    '(complex complex)
    (lambda (z1 z2) (tag (mul-complex z1 z2)))
  )
  (put 
    'div
    '(complex complex)
    (lambda (z1 z2) (tag (div-complex z1 z2)))
  )
  (put 
    'equ?
    '(complex complex)
    (lambda (z1 z2) (equ?-complex z1 z2))
  )
  (put 
    '=zero?
    '(complex)
    (lambda (z) (=zero?-complex z))
  )
  (put 
    'additive-inverse
    '(complex)
    (lambda (z) (additive-inverse-complex z))
  )
  (put 
    'make-from-real-imag
    'complex
    (lambda (x y) (tag (make-from-real-imag x y)))
  )
  (put 
    'make-from-mag-ang
    'complex
    (lambda (r a) (tag (make-from-mag-ang r a)))
  )

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  'done
)
(install-complex-package)
