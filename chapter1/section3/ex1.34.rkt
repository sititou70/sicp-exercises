#lang racket/base

(define (f g) (g 2))

(define 
  (square x)
  (* x x)
)
(f square)
; 4

(f (lambda (z) (* z (+ z 1))))
; 6

; ここで以下を実行するとエラーになる。以下のように評価が進むからである
; (f f)
; (f 2)
; (2 2)
; ここで、2は手続きではないためエラーである
(f f)
