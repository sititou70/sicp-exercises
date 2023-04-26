#lang racket/base

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define 
  (make-tree entry left right)
  (list entry left right)
)

(define 
  (list->tree elements)

  (define 
    (partial-tree elts n)
    (if (= n 0) 
      (cons '() elts)
      (let 
        ((left-size (quotient (- n 1) 2))) ; 左の木の要素数を計算する。これはentryを除いて2で割った商としている
        (let 
          ((left-result  ; 左側の要素数でバランスの取れた木を計算
             (partial-tree elts left-size)
           ) 
          )
          (let 
            ((left-tree (car left-result))  ; 計算の結果、できた木をleft-treeに
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))) ; 右の木の要素数は、左の木とentryの要素数をnから引いたもの
            )
            (let 
              ((this-entry (car non-left-elts))  ; 左の木に含まれなかったもっとも小さい要素をentryに
                (right-result 
                  (partial-tree 
                    (cdr non-left-elts) ; 左の木でもentryでもない要素で右の木を計算
                    right-size
                  )
                )
              )
              (let 
                ((right-tree (car right-result))  ; 計算の結果、できた木をright-treeに
                  (remaining-elts 
                    (cdr right-result)
                  )
                )
                (cons 
                  (make-tree  ; これまでの計算結果を返す
                    this-entry
                    left-tree
                    right-tree
                  )
                  remaining-elts
                )
              )
            )
          )
        )
      )
    )
  )

  (car (partial-tree elements (length elements)))
)

; a. 手続きの説明をコメントとして記述した
; また、以下が生成する木を示す。
(list->tree '(1 3 5 7 9 11))
; '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
; 図示すると次のようになる。
;    5
; 1     9  
;  3   7 11

; b. 計算量について、partial-treeのnが0でない部分の処理が実行される回数で見積もる。
; また、以降は完全にバランスの取れた木を生成する場合を想定する。つまり、list->tree与えられるリストの長さが2^k - 1の場合である。
; この回数は、partial-treeが呼び出されるnによって次のようになる
; T(n) = 0 (n = 0)
;      = 2T((n - 1) / 2) + 1 (otherwise)
; これをn > 0について解くと
; T(n) = 2T((n - 1) / 2) + 1
; T(n) = 2^1{T((n - 1) / 2)} + 1
; T(n) = 2^1{2T((n - 3) / 4) + 1} + 1
; T(n) = 2^2{T((n - 3) / 4)} + 1 + 2
; T(n) = 2^2{2T((n - 7) / 8) + 1} + 1 + 2
; T(n) = 2^3{T((n - 7) / 8)} + 1 + 2 + 4
; T(n) = 2^4{T((n - 15) / 16)} + 1 + 2 + 4 + 8
; T(n) = 2^5{T((n - 31) / 32)} + 1 + 2 + 4 + 8 + 16
; T(n) = 2^5{T((n - (2^5 - 1)) / 2^5)} + 2^5 - 1
; T(n) = 2^{log (n + 1)}{T((n - (2^{log (n + 1)} - 1)) / 2^{log (n + 1)})} + 2^{log (n + 1)} - 1
; T(n) = (n + 1){T((n - (n + 1 - 1)) / (n + 1))} + (n + 1) - 1
; T(n) = (n + 1){T((0 / (n + 1))} + (n + 1) - 1
; T(n) = (n + 1){T(0)} + (n + 1) - 1
; T(n) = (n + 1) - 1
; T(n) = n

; したがって線形オーダー、θ(n)である。
