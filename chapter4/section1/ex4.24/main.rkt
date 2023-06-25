#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require (only-in "m-eval/eval-apply.rkt" [eval analyse-eval]))
(require (only-in "m-eval/eval-apply-orig.rkt" [eval orig-eval]))
(require "m-eval/global-environment.rkt")

; main
(define start 0)
(define end 0)

(displayln "--- factorial ---")
(set! start (current-process-milliseconds))
(orig-eval 
  '((lambda () 
      (define 
        (factorial n)
        (if (= n 1) 1 (* n (factorial (- n 1))))
      )
      (factorial 50000)
      'ok
    )
   )
  the-global-environment
)
(set! end (current-process-milliseconds))
(display "original version m-eval: ")
(display (- end start))
(displayln "ms")

(set! start (current-process-milliseconds))
(analyse-eval 
  '((lambda () 
      (define 
        (factorial n)
        (if (= n 1) 1 (* n (factorial (- n 1))))
      )
      (factorial 50000)
      'ok
    )
   )
  the-global-environment
)
(set! end (current-process-milliseconds))
(display "analyse version m-eval: ")
(display (- end start))
(displayln "ms")

(displayln "--- fib ---")
(set! start (current-process-milliseconds))
(orig-eval 
  '((lambda () 
      (define 
        (fib n)
        (cond 
          ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1)) (fib (- n 2))))
        )
      )
      (fib 30)
      'ok
    )
   )
  the-global-environment
)
(set! end (current-process-milliseconds))
(display "original version m-eval: ")
(display (- end start))
(displayln "ms")

(set! start (current-process-milliseconds))
(analyse-eval 
  '((lambda () 
      (define 
        (fib n)
        (cond 
          ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1)) (fib (- n 2))))
        )
      )
      (fib 30)
      'ok
    )
   )
  the-global-environment
)
(set! end (current-process-milliseconds))
(display "analyse version m-eval: ")
(display (- end start))
(displayln "ms")

; 実行結果は以下のようになった

; --- factorial ---
; original version m-eval: 1810ms
; analyse version m-eval: 1753ms

; --- fib ---
; original version m-eval: 1973ms
; analyse version m-eval: 1040ms

; factorialの実験では、実行時間にほとんど差がない。
; 構文解析のコストは(1 - 1753 / 1810) * 100 = 3.14%程度にとどまった。
; この実験での処理では大きな数値を扱うため、構文解析よりも計算そのもののコストのほうが大きかったと考えられる。

; 一方で、適用の回数が指数的であるフィボナッチの実験では、実行時間に大きな差が発生した
; 構文解析のコストは(1 - 1040 / 1973) * 100 = 47.28%にもなった
; この実験では、階乗計算に比べると比較的小さな数を扱う。加えて、手続きの適用回数が指数的に増加するため、構文解析のコストが大きかったと考える。
