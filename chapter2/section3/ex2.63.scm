#lang racket/base

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define 
  (make-tree entry left right)
  (list entry left right)
)

(define 
  (tree->list-1 tree)
  (if (null? tree) 
    '()
    (append 
      (tree->list-1 (left-branch tree))
      (cons 
        (entry tree)
        (tree->list-1 
          (right-branch tree)
        )
      )
    )
  )
)

(define 
  (tree->list-2 tree)

  (define 
    (copy-to-list tree result-list)
    (if (null? tree) 
      result-list
      (copy-to-list 
        (left-branch tree)
        (cons 
          (entry tree)
          (copy-to-list 
            (right-branch tree)
            result-list
          )
        )
      )
    )
  )

  (copy-to-list tree '())
)

(define 
  tree
  (make-tree 
    3
    (make-tree 1 null null)
    (make-tree 
      7
      (make-tree 5 null null)
      (make-tree 
        9
        null
        (make-tree 11 null null)
      )
    )
  )
)
(tree->list-1 tree)
; '(1 3 5 7 9 11)
(tree->list-2 tree)
; '(1 3 5 7 9 11)

; a. 両方の手続きは同じ結果を返す。

; b. 計算量については両者は同じでない。以降、バランスの取れた木を想定する。
; consのコストを1、n個の要素を追加するappendのコストをnと見積もる

; ノード数nの木を処理するtree->list-1の計算量をTとすると
; T(n) = 2T(n/2) + n/2
;      = 2^1{2T(n/2^2) + n/2^2} + n/2
;      = 2^1{2T(n/2^2)} + 2n/2
;      = 2^2{2T(n/2^3) + n/2^3} + 2n/2
;      = 2^2{2T(n/2^3)} + 3n/2
;      = 2^3{2T(n/2^4)} + 4n/2
;      = 2^4{2T(n/2^5)} + 5n/2
;      ...
;      = 2^{log n}{2T(n/2^{log n} + 1)} + log n * n / 2
;      = 2^{log n}{2T(0)} + log n * n / 2
;      = log n * n / 2
;      <= n log n

; ノード数nの木を処理するtree->list-2の計算量をTとすると
; T(n) = 2T(n/2) + 1
;      = 2^1{2T(n/2^2) + 1} + 1
;      = 2^2{T(n/2^2)} + 1 + 2
;      = 2^2{2T(n/2^3) + 1} + 1 + 2
;      = 2^3{T(n/2^3)} + 1 + 2 + 4
;      = 2^4{T(n/2^4)} + 1 + 2 + 4 + 8
;      = 2^5{T(n/2^5)} + 1 + 2 + 4 + 8 + 16
;      = 2^5{T(n/2^5)} + 2^5 - 1
;      ...
;      = 2^{log n + 1}{T(n/2^{log n + 1})} + 2^{log n + 1} - 1
;      = 2^{log n + 1}{T(0)} + 2^{log n + 1} - 1
;      = 2^{log n + 1} - 1
;      = 2^{log n} * 2^1 - 1
;      = 2n - 1
;      <= n

; したがって、tree->list-1の方が遅い
