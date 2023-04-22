#lang racket/base

(car ' 'abracadabra)
; 'quote

; 上記のプログラムは以下のように解釈できる
; (car '(quote abracadabra))
; 'quote
