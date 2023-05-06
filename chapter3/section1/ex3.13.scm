#lang sicp

(define 
  (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x)))
)
(define 
  (make-cycle x)
  (set-cdr! (last-pair x) x)
  x
)

(define z (make-cycle (list 'a 'b 'c)))
; 箱とポインタの図
; z -> [.|.]-> [.|.]-> [.|.]-> [.|/]
; ^     |       |       |       |
; |     v       v       v       |
; |     a       b       c       |
; |                             |
; +-----------------------------+

; 停止しない
(last-pair z)
