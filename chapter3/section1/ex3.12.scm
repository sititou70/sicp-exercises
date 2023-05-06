#lang sicp

(define 
  (append x y)
  (if (null? x) 
    y
    (cons (car x) (append (cdr x) y))
  )
)

(define 
  (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x)))
)
(define 
  (append! x y)
  (set-cdr! (last-pair x) y)
  x
)

(define x (list 'a 'b))
(define y (list 'c 'd))
; 箱とポインタの図
; x -> [.|.]-> [.|/]  y -> [.|.]-> [.|/]
;       |       |           |       |
;       v       v           v       v
;       a       b           c       d

(define z (append x y))
z
; (a b c d)
(cdr x)
; (b)

; 箱とポインタの図
; x -> [.|.]-> [.|/]  y -> [.|.]-> [.|/]
;       |       |           |       |
;       v       v           v       v
;       a       b           c       d
;       ^       ^           ^       ^
;       |       |           |       |
; z -> [.|.]-> [.|.]-----> [.|.]-> [.|/]

(define w (append! x y))
w
; (a b c d)
(cdr x)
; (b c d)

; 箱とポインタの図
; x -> [.|.]-> [.|.]-> y -> [.|.]-> [.|/]
; ^     |       |            |       |
; |     v       v            v       v
; z     a       b            c       d
