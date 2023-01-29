#lang racket/base
; https://planet.racket-lang.org/display.ss?package=science.plt&owner=williams
; sudo raco planet install "williams" "science.plt" 4 8
(require (planet williams/science:4:8/science))

; nが素数なら#tを返す
(define 
  (mrtest n iter-num)

  (define 
    (square x)
    (* x x)
  )

  ; base^m modulo mを計算する．ただし，mを法とする非自明な1の平方根が見つかった場合は0を返す
  ; 補足
  ; mが素数であれば，mを法とする非自明な1の平方根は1とm - 1しかありえない．これらを自明な平方根と呼ぶ．証明：https://ja.wikipedia.org/wiki/%E3%83%9F%E3%83%A9%E3%83%BC%E2%80%93%E3%83%A9%E3%83%93%E3%83%B3%E7%B4%A0%E6%95%B0%E5%88%A4%E5%AE%9A%E6%B3%95
  ; したがって，それ以外の平方根が見つかった場合はmは合成数であり，expmodはtestを失敗させるために0を返す
  (define 
    (expmod base exp m)
    (cond 
      ((= exp 0) 1)
      ((even? exp) ; base^m modulo mを，(base^(m/2) modulo m)^2 modulo mで求める
       (let 
         ((sqrt-for-result (expmod base (/ exp 2) m))) ; base^(m/2) modulo mの部分

         (let 
           ((result  ; (base^(m/2) modulo m)^2 modulo mの部分
              (remainder 
                (square sqrt-for-result)
                m
              )
            ) 
           )

           (if 
             (and 
               (= result 1) ; sqrt-for-resultがmを法とする1の平方根である（sqrt-for-resultの2乗をmで割った余りが1である）
               (and (not (= sqrt-for-result 1)) (not (= sqrt-for-result (- m 1)))) ; sqrt-for-resultはmを法とする自明な1の平方根ではない
             )
             0
             result
           )
         )
       )
      )
      (else
       (remainder 
         (* base (expmod base (- exp 1) m))
         m
       )
      )
    )
  )

  (define 
    (test a)
    (= (expmod a n n) a)
  )

  (define 
    (iter count)
    (if (= count iter-num) 
      #t
      (if (test count) 
        (iter (+ count 1))
        #f
      )
    )
  )

  (iter 1)
)

; 合成数．いずれも#fを表示する
(display "合成数\n")
(mrtest 100000000000033 10000)
(mrtest 100000000000069 10000)
(mrtest 100000000000095 10000)
(mrtest 1000000000000039 10000)
(mrtest 1000000000000093 10000)
(mrtest 1000000000000157 10000)
(mrtest 10000000000000063 10000)
(mrtest 10000000000000067 10000)
(mrtest 10000000000000077 10000)
(mrtest 100000000000000001 10000)
(mrtest 100000000000000016 10000)
(mrtest 100000000000000017 10000)
(display "\n")

; 素数．いずれも#tを表示する
(display "素数\n")
(mrtest 100000000000031 10000)
(mrtest 100000000000067 10000)
(mrtest 100000000000097 10000)
(mrtest 1000000000000037 10000)
(mrtest 1000000000000091 10000)
(mrtest 1000000000000159 10000)
(mrtest 10000000000000061 10000)
(mrtest 10000000000000069 10000)
(mrtest 10000000000000079 10000)
(mrtest 100000000000000003 10000)
(mrtest 100000000000000013 10000)
(mrtest 100000000000000019 10000)
(display "\n")

; カーマイケル数．いずれも合成数．ミラーラビンテストでは騙されず#fを表示する．
(display "カーマイケル数\n")
(mrtest 561 10000)
(mrtest 1105 10000)
(mrtest 1729 10000)
(mrtest 2465 10000)
(mrtest 2821 10000)
(mrtest 6601 10000)
(display "\n")
