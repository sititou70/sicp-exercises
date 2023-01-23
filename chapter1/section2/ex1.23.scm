#lang racket/base

(define 
  (square x)
  (* x x)
)

(define (smallest-divisor n) (find-divisor n 2))
(define 
  (find-divisor n test-divisor)
  (cond 
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (next test-divisor)))
  )
)
(define (divides? a b) (= (remainder b a) 0))
(define 
  (next x)
  (if (= x 2) 3 (+ x 2))
)

(define 
  (prime? n)
  (= n (smallest-divisor n))
)

(define 
  (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-process-milliseconds))
)
(define 
  (start-prime-test n start-time)
  (if (prime? n) 
    (report-prime (- (current-process-milliseconds) start-time))
    null
  )
)
(define 
  (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
)

(timed-prime-test 100000000000031)
(timed-prime-test 100000000000067)
(timed-prime-test 100000000000097)
(timed-prime-test 1000000000000037)
(timed-prime-test 1000000000000091)
(timed-prime-test 1000000000000159)
(timed-prime-test 10000000000000061)
(timed-prime-test 10000000000000069)
(timed-prime-test 10000000000000079)
(timed-prime-test 100000000000000003)
(timed-prime-test 100000000000000013)
(timed-prime-test 100000000000000019)

; 以前の計測結果
; 100000000000031 *** 131
; 100000000000067 *** 130
; 100000000000097 *** 133
; 1000000000000037 *** 417
; 1000000000000091 *** 405
; 1000000000000159 *** 405
; 10000000000000061 *** 1281
; 10000000000000069 *** 1279
; 10000000000000079 *** 1283
; 100000000000000003 *** 4272
; 100000000000000013 *** 4279
; 100000000000000019 *** 4275

; 今回の計測結果
; 100000000000031 *** 67
; 100000000000067 *** 67
; 100000000000097 *** 66
; 1000000000000037 *** 209
; 1000000000000091 *** 209
; 1000000000000159 *** 209
; 10000000000000061 *** 660
; 10000000000000069 *** 660
; 10000000000000079 *** 658
; 100000000000000003 *** 2100
; 100000000000000013 *** 2080
; 100000000000000019 *** 2112

; 実行速度は以前のおおよそ半分になっており，予想通りである．
