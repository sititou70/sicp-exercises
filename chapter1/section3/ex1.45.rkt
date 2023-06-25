#lang racket/base
; 以下のように実行する
; racket ex1.45.rkt > ex1.45.md

(define 
  (compose f g)
  (lambda (x) (f (g x)))
)

(define 
  (repeated f n)

  (cond 
    ((= n 1) f)
    ((> n 1) (compose f (repeated f (- n 1))))
    (else (error "Argument n must be positive and non-zero value."))
  )
)

(define 
  (average x y)
  (/ (+ x y) 2)
)

(define tolerance 0.00001)
(define try-limit 1000)
(define 
  (fixed-point f first-guess)

  (define 
    (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance
    )
  )

  (define 
    (try guess cnt)
    (let 
      ((next (f guess)))
      (cond 
        ((> cnt try-limit) null)
        ((close-enough? guess next)
         next
        )
        (else (try next (+ cnt 1)))
      )
    )
  )

  (try first-guess 1)
)

(define 
  (average-damp f)
  (lambda (x) (average x (f x)))
)

; 汎用的なn乗根を求める手続き
; n: n乗根のn
; c: ルートの中身
(define 
  (root n c)

  (fixed-point 
    ((repeated 
       average-damp
       (floor (log n 2))
     ) 
      (lambda (y) 
        (/ 
          c
          (expt y (- n 1))
        )
      )
    )
    1.0
  )
)


(display "まず、n乗根の不動点探索と、平均緩和の回数の関係について観察する。\n\n")
(display "ここでは、2のn乗根を様々な緩和回数で近似する。近似の回数が1000回を超えても収束しないものは発散するとみなす。\n\n")

; ルートの中身。被開平数
(define C 2)
; 調査対象が発散するか調べる
; n: n乗根のn
; damp-cnt: 平均緩和を行う回数
(define 
  (display-divergence n damp-cnt)

  (let 
    ((result 
       (fixed-point 
         ((repeated average-damp damp-cnt) (lambda (y) (/ C (expt y (- n 1)))))
         1.0
       )
     ) 
    )
    (if (null? result) 
      (display "❌発散|")
      (display "✅収束|")
    )
  )
)

(display 
  "
|   |1回緩和|2回緩和|3回緩和|4回緩和|5回緩和|
|:-:|:-----:|:-----:|:-----:|:-----:|:-----:|
"
)

(display "|*2乗根|")
(display-divergence 2 1)
(display-divergence 2 2)
(display-divergence 2 3)
(display-divergence 2 4)
(display-divergence 2 5)
(newline)

(display "|3乗根|")
(display-divergence 3 1)
(display-divergence 3 2)
(display-divergence 3 3)
(display-divergence 3 4)
(display-divergence 3 5)
(newline)

(display "|*4乗根|")
(display-divergence 4 1)
(display-divergence 4 2)
(display-divergence 4 3)
(display-divergence 4 4)
(display-divergence 4 5)
(newline)

(display "|5乗根|")
(display-divergence 5 1)
(display-divergence 5 2)
(display-divergence 5 3)
(display-divergence 5 4)
(display-divergence 5 5)
(newline)

(display "|7乗根|")
(display-divergence 7 1)
(display-divergence 7 2)
(display-divergence 7 3)
(display-divergence 7 4)
(display-divergence 7 5)
(newline)

(display "|*8乗根|")
(display-divergence 8 1)
(display-divergence 8 2)
(display-divergence 8 3)
(display-divergence 8 4)
(display-divergence 8 5)
(newline)

(display "|9乗根|")
(display-divergence 9 1)
(display-divergence 9 2)
(display-divergence 9 3)
(display-divergence 9 4)
(display-divergence 9 5)
(newline)

(display "|15乗根|")
(display-divergence 15 1)
(display-divergence 15 2)
(display-divergence 15 3)
(display-divergence 15 4)
(display-divergence 15 5)
(newline)

(display "|*16乗根|")
(display-divergence 16 1)
(display-divergence 16 2)
(display-divergence 16 3)
(display-divergence 16 4)
(display-divergence 16 5)
(newline)

(display "|17乗根|")
(display-divergence 17 1)
(display-divergence 17 2)
(display-divergence 17 3)
(display-divergence 17 4)
(display-divergence 17 5)
(newline)

(display "|31乗根|")
(display-divergence 31 1)
(display-divergence 31 2)
(display-divergence 31 3)
(display-divergence 31 4)
(display-divergence 31 5)
(newline)

(display "|*32乗根|")
(display-divergence 32 1)
(display-divergence 32 2)
(display-divergence 32 3)
(display-divergence 32 4)
(display-divergence 32 5)
(newline)
(newline)

(display "結果として、n乗根が収束するにはlog n回の平均緩和が必要だとわかる。\n\n")
(display "これを元に、root手続きを実装できる。この手続きによって次のように値を計算できる。\n\n")

(display "```\n")

(display "7の7乗根 = ")
(display (root 7 7))
(newline)

(display "54321の31乗根 = ")
(display (root 31 54321))
(newline)

(display "99999999の123乗根 = ")
(display (root 123 99999999))
(newline)

(display "```\n")
