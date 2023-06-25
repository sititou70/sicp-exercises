#lang racket/base

; チャーチ数nは、手続きfと値xを引数にとって、xにfをn回適用したものを返す手続きと考えられる
; 補足：ラムダ計算においては、fとxは両方ともラムダ抽象である
(define zero (lambda (f) (lambda (x) x)))
(define 
  (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x))))
)

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

(define 
  (add n1 n2)
  (lambda (f) (lambda (x) ((n2 f) ((n1 f) x))))
)

; チャーチ数を通常の数値に変換する
(define 
  (c2n n)

  (define 
    (inc n)
    (+ n 1)
  )

  ((n inc) 0)
)

(c2n zero)
; 0
(c2n (add-1 zero))
; 1
(c2n (add-1 (add-1 zero)))
; 2
(c2n (add-1 (add-1 (add-1 zero))))
; 3

(c2n one)
; 1
(c2n two)
; 2
(c2n three)
; 3

(c2n (add zero one))
; 1
(c2n (add one one))
; 2
(c2n (add two three))
; 5
