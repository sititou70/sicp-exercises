#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "m-eval/eval-apply.rkt")
(require "m-eval/global-environment.rkt")
(require "m-eval/environment.rkt")
(require "m-eval/procedure.rkt")

; ドライバループは標準入力に依存しており、検証が簡単ではない
; そのため、入力をコードから受け付けるようにする
(define 
  (l-eval input)

  (define 
    (car-meta-list meta-list)
    (force-it 
      (lookup-variable-value 'x (procedure-environment (cdr meta-list)))
    )
  )
  (define 
    (cdr-meta-list meta-list)
    (force-it 
      (lookup-variable-value 'y (procedure-environment (cdr meta-list)))
    )
  )
  (define 
    (eval-meta-list meta-list max-num)
    (cond 
      ((null? meta-list) '())
      ((= max-num 0) (cons "..." '()))
      (else
       (cons 
         (car-meta-list meta-list)
         (eval-meta-list (cdr-meta-list meta-list) (- max-num 1))
       )
      )
    )
  )

  (let 
    ( ;
     (result (actual-value input the-global-environment))
    )

    (if 
      (and 
        (pair? result)
        (eq? (car result) 'pair)
      )
      (displayln (eval-meta-list result 5))
      result
    )
  )
)

; cons car cdr
(l-eval '(define orig-cons cons))
; 'ok
(l-eval '(define orig-car car))
; 'ok
(l-eval '(define orig-cdr cdr))
; 'ok

(l-eval 
  '(define
    (cons x y)
    (orig-cons 
      'pair
      (lambda (m) (m x y))
    )
   )
)
; 'ok
(l-eval 
  '(define
    (car z)
    ((orig-cdr z) 
      (lambda (p q) p)
    )
   )
)
; 'ok
(l-eval 
  '(define
    (cdr z)
    ((orig-cdr z) 
      (lambda (p q) q)
    )
   )
)
; 'ok

; main
; 有限リスト
(l-eval '(define l (cons 1 (cons 2 (cons 3 '())))))
; 'ok
(l-eval '(car (cdr l)))
; 2
(l-eval 'l)
; {1 2 3}

; 無限リスト
(l-eval '(define ones (cons 1 ones)))
; 'ok
(l-eval 'ones)
; {1 1 1 1 1 ...}

; 通常の手続きは今までどおり動作する
(l-eval 
  '(define (double x) (+ x x))
)
; 'ok
(l-eval 
  '(double 2)
)
; 4
