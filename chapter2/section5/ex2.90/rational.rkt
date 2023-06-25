#lang racket/base

(require "data-directed-utils.rkt")

; --------
; 有理数算術演算パッケージ
; --------

(define 
  (install-rational-package)

  ;; 内部手続き
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define 
    (make-rat n d)
    (let 
      ((g (gcd n d)))
      (cons (/ n g) (/ d g))
    )
  )
  (define 
    (add-rat x y)
    (make-rat 
      (+ (* (numer x) (denom y)) 
         (* (numer y) (denom x))
      )
      (* (denom x) (denom y))
    )
  )
  (define 
    (sub-rat x y)
    (make-rat 
      (- (* (numer x) (denom y)) 
         (* (numer y) (denom x))
      )
      (* (denom x) (denom y))
    )
  )
  (define 
    (mul-rat x y)
    (make-rat 
      (* (numer x) (numer y))
      (* (denom x) (denom y))
    )
  )
  (define 
    (div-rat x y)
    (make-rat 
      (* (numer x) (denom y))
      (* (denom x) (numer y))
    )
  )
  (define 
    (equ?-rat x y)
    (= 
      (* (numer x) (denom y))
      (* (denom x) (numer y))
    )
  )
  (define 
    (=zero?-rat x)
    (= 
      (/ (numer x) (denom x))
      0
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
    'make
    'rational
    (lambda (n d) (tag (make-rat n d)))
  )

  'done
)
(install-rational-package)
