#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require (only-in "m-eval/eval-apply.scm" actual-value))
(require 
  (only-in "m-eval/eval-apply-nomemo.scm" [actual-value actual-value-nomemo])
)
(require "m-eval/global-environment.scm")

(define a-val actual-value)
(define a-val-nomemo actual-value-nomemo)

; main
(a-val '(define count 0) the-global-environment)
; 'ok
(a-val 
  '(define
    (id x)
    (set! count (+ count 1))
    x
   )
  the-global-environment
)
; 'ok
(a-val 
  '(define
    (square x)
    (* x x)
   )
  the-global-environment
)
; 'ok

; メモ化が有効な場合
(a-val '(square (id 10)) the-global-environment)
; 100
(a-val 'count the-global-environment)
; 1
; (id 10)の値はメモ化された遅延オブジェクトとして1回だけ評価されるため、countは1

(a-val-nomemo '(set! count 0) the-global-environment)
; 'ok

; メモ化が無効な場合
(a-val-nomemo '(square (id 10)) the-global-environment)
; 100
(a-val-nomemo 'count the-global-environment)
; 2
; (id 10)はメモ化されないためsquareによって2回評価される。よってcountは2

; squareのように遅延オブジェクトを何度も評価するような場合、メモ化の有無によって実行時間に差が生まれるのと考えられる。
; ここでは、足し算によって掛け算を実行するmul手続きについて2^7の計算について考えてみる
(define start 0)
(define end 0)
(a-val 
  '(define
    (mul x y)
    (set! count (+ count 1))
    (if (= y 0) 
      0
      (+ 
        x
        (mul x (- y 1))
      )
    )
   )
  the-global-environment
)
; 'ok

; メモ化が有効な場合
(a-val '(set! count 0) the-global-environment)

(set! start (current-process-milliseconds))
(a-val 
  '(mul 2 (mul 2 (mul 2 (mul 2 (mul 2 (mul 2 2))))))
  the-global-environment
)
; 128
(set! end (current-process-milliseconds))
(display (- end start))
(displayln "ms")
; 1ms

(a-val-nomemo 
  'count
  the-global-environment
)
; 132
; 計算時間とmulの再帰回数は少ない

; 一方、メモ化が無効な場合は、
(a-val '(set! count 0) the-global-environment)

(set! start (current-process-milliseconds))
(a-val-nomemo 
  '(mul 2 (mul 2 (mul 2 (mul 2 (mul 2 (mul 2 2))))))
  the-global-environment
)
; 128
(set! end (current-process-milliseconds))
(display (- end start))
(displayln "ms")
; 8266ms

(a-val-nomemo 
  'count
  the-global-environment
)
; 6930560
; 計算時間とmulの再帰回数は非常に多くなる。
; mul内で引数を評価するたびに、その中で遅延化されたmulの呼び出しが発生するためである。
