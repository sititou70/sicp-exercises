#lang racket/base

(define 
  (subsets s)
  (if (null? s) 
    (list null)
    (let 
      ((rest (subsets (cdr s))))
      (append 
        rest
        (map 
          (lambda (subset) 
            (cons 
              (car s)
              subset
            )
          )
          rest
        )
      )
    )
  )
)

(subsets (list 1 2 3))
; '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

; 集合A = (1, 2, 3)を考える。
; 集合Aのsubsetは、それぞれの要素を含むか含まないかによって決まるから、要素を含むときを1、含まないときを0とすると、次のように表せる

; 1 2 3
; -----
; 0 0 0 = ()
; 0 0 1 = (3)
; 0 1 0 = (2)
; 0 1 1 = (2 3)
; 1 0 0 = (1)
; 1 0 1 = (1 3)
; 1 1 0 = (1 2)
; 1 1 1 = (1 2 3)

; ここで、1を含まないsubsetの集合（上4つ分）は、A - 1（集合Aから要素1を除いたもの、つまり(2 3)）のsubsetsと同じだとわかる。
; また、1を含むsubsetの集合（下4つ分）から1を除くと、1を含まないsubsetの集合（A - 1のsubsets）と同じになることがわかる。
; つまり、集合Aのsubsetsは、『「A - 1のsubsets」と「A - 1のsubsetsの各要素に1を加えたもの」の和集合』となる。
; これを先頭の要素から再帰的に行っていくことでsubsetsは動作する
