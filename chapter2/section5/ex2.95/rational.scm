#lang racket/base

(require "data-directed-utils.scm")
(require "generic-procs.scm")

; --------
; 有理数算術演算パッケージ
; --------

(define 
  (install-rational-package)

  ;; 内部手続き
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d) (cons n d))
  (define 
    (add-rat x y)
    (make-rat 
      (add 
        (mul (numer x) (denom y))
        (mul (numer y) (denom x))
      )
      (mul (denom x) (denom y))
    )
  )
  (define 
    (sub-rat x y)
    (make-rat 
      (sub 
        (mul (numer x) (denom y))
        (mul (numer y) (denom x))
      )
      (mul (denom x) (denom y))
    )
  )
  (define 
    (mul-rat x y)
    (make-rat 
      (mul (numer x) (numer y))
      (mul (denom x) (denom y))
    )
  )
  (define 
    (div-rat x y)
    (make-rat 
      (mul (numer x) (denom y))
      (mul (denom x) (numer y))
    )
  )
  (define 
    (equ?-rat x y)
    (equ? 
      (mul (numer x) (denom y))
      (mul (denom x) (numer y))
    )
  )
  (define 
    (=zero?-rat x)
    (=zero? (numer x))
  )
  (define 
    (additive-inverse-rat x)
    (make-rat 
      (additive-inverse (numer x))
      (denom x)
    )
  )

  ;; システムのほかの部分とのインターフェイス
  (define (tag x) (attach-tag 'rational x))

  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)

  (put 
    'add
    '(rational rational)
    (lambda (x y) (tag (add-rat x y)))
  )
  (put 
    'sub
    '(rational rational)
    (lambda (x y) (tag (sub-rat x y)))
  )
  (put 
    'mul
    '(rational rational)
    (lambda (x y) (tag (mul-rat x y)))
  )
  (put 
    'div
    '(rational rational)
    (lambda (x y) (tag (div-rat x y)))
  )
  (put 
    'equ?
    '(rational rational)
    (lambda (x y) (equ?-rat x y))
  )
  (put 
    '=zero?
    '(rational)
    (lambda (x) (=zero?-rat x))
  )
  (put 
    'additive-inverse
    '(rational)
    (lambda (x) (tag (additive-inverse-rat x)))
  )
  (put 
    'make
    'rational
    (lambda (n d) (tag (make-rat n d)))
  )

  'done
)
(install-rational-package)
