#lang racket/base

(require "generic-procs.scm")
(require "scheme-number.scm")
(require "rational.scm")
(require "complex.scm")
(require "polynomial.scm")
(require "coercion.scm")

(define p1 (make-polynomial 'x '((2 1) (1 -1) (0 1))))
(define p2 (make-polynomial 'x '((2 11) (0 7))))
(define p3 (make-polynomial 'x '((1 13) (0 5))))
(define q1 (mul p1 p2))
(define q2 (mul p1 p3))
(greatest-common-divisor q1 q2)
; >(gcd-terms '((4 11) (3 -11) (2 18) (1 -7) (0 7)) '((3 13) (2 -8) (1 8) (0 5)))
; >(gcd-terms
;   '((3 13) (2 -8) (1 8) (0 5))
;   '((2 1458/169) (1 -1458/169) (0 1458/169)))
; >(gcd-terms '((2 1458/169) (1 -1458/169) (0 1458/169)) '())
; <'((2 1458/169) (1 -1458/169) (0 1458/169))
; '(polynomial x (2 1458/169) (1 -1458/169) (0 1458/169))

; GCDの計算の途中で係数膨張がおこっている
