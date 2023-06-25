#lang racket/base

(require "generic-procs.rkt")
(require "scheme-number.rkt")
(require "rational.rkt")
(require "complex.rkt")
(require "polynomial.rkt")
(require "coercion.rkt")

(add 
  (make-rational 1 2)
  (make-rational 1 4)
)
; '(rational 3 . 4)
; 既約になっている

(define p1 (make-polynomial 'x '((1 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 -1))))
(define p3 (make-polynomial 'x '((1 1))))
(define p4 (make-polynomial 'x '((2 1) (0 -1))))
(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))
(add rf1 rf2)
; '(rational 
;   (polynomial x (3 -1) (2 -2) (1 -3) (0 -1))
;    polynomial x (4 -1) (3 -1) (1 1) (0 1))
; 正しく既約になっている。マイナス符号が多いのが気になるが、これは処理の要件として組み込んでいないため当然である。

; ex2.93のテスト
(define p5 (make-polynomial 'x '((2 1) (0 1))))
(define p6 (make-polynomial 'x '((3 1) (0 1))))
(define rf (make-rational p5 p6))
(add rf rf)
; '(rational 
;   (polynomial x (2 2) (0 2))
;    polynomial x (3 1) (0 1))
; やはり正しく既約になっている。

; ex2.93の環境では以下のような結果だった。
; '(rational 
;   (polynomial x (5 2) (3 2) (2 2) (0 2))
;    polynomial x (4 1) (2 2) (0 1))

