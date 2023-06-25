#lang racket/base
; https://planet.racket-lang.org/display.ss?package=science.plt&owner=williams
; raco planet install "williams" "science.plt" 4 8
(require (planet williams/science:4:8/science))

(define 
  (square x)
  (* x x)
)

(define 
  (expmod base exp m)
  (cond 
    ((= exp 0) 1)
    ((even? exp)
     (remainder 
       (square (expmod base (/ exp 2) m))
       m
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
  (fermat-test n)
  (define 
    (try-it a)
    (= (expmod a n n) a)
  )
  (try-it (+ 1 (random-integer (- n 1))))
)

(define 
  (fast-prime? n times)
  (cond 
    ((= times 0) #t)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else #f)
  )
)

(define 
  (timed-prime-test n times)
  (newline)
  (display n)
  (start-prime-test n times (current-process-milliseconds))
)
(define 
  (start-prime-test n times start-time)
  (if (fast-prime? n times) 
    (report-prime (- (current-process-milliseconds) start-time))
    null
  )
)
(define 
  (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
)

; 練習問題 1.22で見つけた 12 個の素数をそれぞれテストせよ
(timed-prime-test 100000000000031 10000)
(timed-prime-test 100000000000067 10000)
(timed-prime-test 100000000000097 10000)
(timed-prime-test 1000000000000037 10000)
(timed-prime-test 1000000000000091 10000)
(timed-prime-test 1000000000000159 10000)
(timed-prime-test 10000000000000061 10000)
(timed-prime-test 10000000000000069 10000)
(timed-prime-test 10000000000000079 10000)
(timed-prime-test 100000000000000003 10000)
(timed-prime-test 100000000000000013 10000)
(timed-prime-test 100000000000000019 10000)
; 100000000000031 *** 80
; 100000000000067 *** 106
; 100000000000097 *** 75
; 1000000000000037 *** 91
; 1000000000000091 *** 94
; 1000000000000159 *** 96
; 10000000000000061 *** 96
; 10000000000000069 *** 97
; 10000000000000079 *** 97
; 100000000000000003 *** 104
; 100000000000000013 *** 109
; 100000000000000019 *** 106

; 1,000,000 に近い素数をテストするのにかかる時間は 1000 に近い素数をテストするのに必要な時間と比べてどの程度になると予想できるか
; →2倍になると予測できる
(timed-prime-test 1019 1000000)
(timed-prime-test 1000003 1000000)
; 1019 *** 642
; 1000003 *** 1014
; 実際ほぼそのとおりになっている
