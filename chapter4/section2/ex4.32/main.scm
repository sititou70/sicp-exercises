#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "m-eval/eval-apply.scm")
(require "m-eval/global-environment.scm")

(define a-val actual-value)

; main
; cons car cdr
(a-val 
  '(define (cons x y) (lambda (m) (m x y)))
  the-global-environment
)
; 'ok
(a-val 
  '(define (car z) (z (lambda (p q) p)))
  the-global-environment
)
; 'ok
(a-val 
  '(define (cdr z) (z (lambda (p q) q)))
  the-global-environment
)
; 'ok

; list utils
(a-val 
  '(define
    (list-ref items n)
    (if (= n 0) 
      (car items)
      (list-ref (cdr items) (- n 1))
    )
   )
  the-global-environment
)
; 'ok

(a-val 
  '(define
    (map proc items)
    (if (null? items) 
      '()
      (cons 
        (proc (car items))
        (map proc (cdr items))
      )
    )
   )
  the-global-environment
)
; 'ok

(a-val 
  '(define
    (scale-list items factor)
    (map (lambda (x) (* x factor)) items)
   )
  the-global-environment
)
; 'ok

(a-val 
  '(define
    (add-lists list1 list2)
    (cond 
      ((null? list1) list2)
      ((null? list2) list1)
      (else
       (cons 
         (+ (car list1) (car list2))
         (add-lists (cdr list1) (cdr list2))
       )
      )
    )
   )
  the-global-environment
)
; 'ok

(a-val 
  '(define
    (integral integrand initial-value dt)
    (define 
      int
      (cons 
        initial-value
        (add-lists (scale-list integrand dt) int)
      )
    )
    int
   )
  the-global-environment
)
; 'ok

(a-val 
  '(define
    (solve f y0 dt)
    (define y (integral dy y0 dt))
    (define dy (map f y))
    y
   )
  the-global-environment
)
; 'ok

; 自然対数の底を近似してみる
(a-val 
  '(list-ref (solve (lambda (x) x) 1 0.0001) 10000)
  the-global-environment
)
; 2.7181459268252266

; 「より遅延化された」リストの効果を示す例を2つ挙げる。
; 1つは、より遅延化されたリストでは、途中の要素を計算せずに先の要素を取得できるというものである。
(a-val 
  '(define
    (infinity-loop)
    (displayln "loop...")
    (infinity-loop)
   )
  the-global-environment
)
; 'ok
(a-val 
  '(define
    l
    (cons 'a (cons (infinity-loop) (cons 'b '())))
   )
  the-global-environment
)
; 'ok
(a-val 
  '(car l)
  the-global-environment
)
; 'a
(a-val 
  '(car (cdr (cdr l)))
  the-global-environment
)
; 'b
; 従来のストリームでは、リストの先頭の要素は常に計算されていなければならず、途中の要素を飛ばして次の要素を取得することはできなかった

; もう1つは、「無限ツリー」が構築できるというものである。
(a-val 
  '(define
    t
    (cons t t)
   )
  the-global-environment
)
; 'ok
(a-val 
  '(car (car (cdr (cdr (car (cdr (cdr (car (car (cdr (cdr (cdr t))))))))))))
  the-global-environment
)
; (mcons 'procedure (mcons ...
; このツリーはすべての要素が自分自身である。そのため、carとcdrをどのように組み合わせても最初と同じツリーが得られる。
; これは、consのcar側の要素が遅延されていなかった従来の環境では実現できなかったことである。
