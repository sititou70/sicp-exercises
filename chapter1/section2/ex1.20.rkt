#lang racket/base

(define 
  (gcd a b)
  (if (= b 0) 
    a
    (gcd 
      b
      (begin 
        (display "remainder実行\n")
        (remainder a b)
      )
    )
  )
)

(gcd 206 40)

; 正規順序評価規則では，インタプリタは「値が必要になるまで被演算子を評価しない」．
; したがって，以下のようなプロセスが生成される．また，remainderの実行回数も併記する
; (gcd 206 40)
; (if (= 40 0) 206 (gce 40 (remainder 206 40)))
; (gce 40 (remainder 206 40))
; (if (= (remainder 206 40) 0) 40 (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))) ; 1回実行
; (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
; (if (= (remainder 40 (remainder 206 40)) 0) (remainder 206 40) (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) ; 2回実行
; (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
; (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) (remainder 40 (remainder 206 40)) (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))) ; 4回実行
; (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))) ; 7回実行
; (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) ; 2回実行
; (remainder 6 (remainder 40 6)) ; 1回実行
; (remainder 6 4) ; 1回実行
; 2
; remainderは18回実行された


; 適用順序評価規則では，「インタプリタはまず演算子と被演算子を評価」する．
; したがって，以下のようなプロセスが生成される．また，remainderの実行回数も併記する
; (gcd 206 40)
; (if (= 40 0) 206 (gce 40 (remainder 206 40)))
; (gce 40 (remainder 206 40)) ; 1回実行
; (gce 40 6)
; (if (= 6 0) 40 (gcd 6 (remainder 40 6)))
; (gcd 6 (remainder 40 6)) ; 1回実行
; (gcd 6 4)
; (if (= 4 0) 6 (gcd 4 (remainder 6 4)))
; (gcd 4 (remainder 6 4)) ; 1回実行
; (gcd 4 2)
; (if (= 2 0) 4 (gcd 2 (remainder 4 2)))
; (gcd 2 (remainder 4 2)) ; 1回実行
; (gcd 2 0)
; (if (= 0 0) 2 (gcd 0 (remainder 2 0)))
; 2
; remainderは4回実行された
