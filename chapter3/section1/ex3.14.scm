#lang sicp

(define 
  (mystery x)
  (define 
    (loop x y)
    (if (null? x) 
      y
      (let 
        ((temp (cdr x)))
        (set-cdr! x y)
        (loop temp x)
      )
    )
  )
  (loop x '())
)

(define v '(a b c d))
(define w (mystery v))

v
w
; (a)
; (d c b a)

; 箱とポインタの図
; w -> [.|.]-> [.|.]-> [.|.]-> v -> [.|/]
;       |       |       |            |
;       v       v       v            v
;       d       c       b            a

; 一般に、mystery手続きは与えられたリストを逆順に並べ替える
