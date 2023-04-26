#lang racket/base

(define 
  (element-of-set? x set)
  (cond 
    ((null? set) #f)
    ((equal? x (car set)) #t)
    (else (element-of-set? x (cdr set)))
  )
)
(element-of-set? 'a '(a b))
; #t
; この手続きの計算量は、重複を許さない場合と同じである

(define 
  (adjoin-set x set)
  (cons x set)
)
(adjoin-set 'c '(a b))
; '(c a b)
; この手続きの計算量は、Θ(1)である。重複を許さなかった場合はΘ(n)であった。
; 重複を許すことによって、xがすでに集合に含まれてるかを判定する必要がなくなったため、計算量が減少した。

(define 
  (union-set set1 set2)
  (append set1 set2)
)
(union-set '(a b d) '(b c d))
; '(a b d b c d)
; この手続きの計算量は、Θ(n)である。重複を許さなかった場合はΘ(n^2)であった。
; 重複を許すことによって、単に2つの集合を結合することで和集合を求められる

(define 
  (intersection-set set1 set2)
  (cond 
    ((or (null? set1) (null? set2)) '())
    ((element-of-set? (car set1) set2)
     (cons (car set1) (intersection-set (cdr set1) set2))
    )
    (else (intersection-set (cdr set1) set2))
  )
)
(intersection-set '(a a b b d d) '(b b c c d d))
; '(b d)
; この手続きの計算量はΘ(n^2)である。重複を許さなかった場合もΘ(n^2)であった。
; 積集合を求めるためには、一方の集合の各要素が、他方の集合に含まれるかを判定する必要がある、
; そのためelement-of-set?を使用する必要があるのだが、この計算量が重複の有無によって変化しないため、intersection-setの計算量も変化しない。

; 全体として、要素の重複を許す場合、adjoin-setやunion-setが早い。
; したがって、これらの「データを追加する」操作を頻繁に行うようなシナリオでは、要素の重複を許す表現が有利である。
