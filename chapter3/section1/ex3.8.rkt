#lang racket/base

; 呼ばれた回数 * xを返す
(define 
  (make-f)
  (define count 0)
  (lambda (x) 
    (set! count (+ count 1))
    (* count x)
  )
)
(define f (make-f))

(+ (f 0) (f 1))
; 2
; (f 0)の次に(f 1)が呼ばれた
; したがって、今回の処理系では部分式を左から評価しているためと考えられる

; 逆だと
(define f2 (make-f))
(+ (f2 1) (f2 0))
; 1
; となる
