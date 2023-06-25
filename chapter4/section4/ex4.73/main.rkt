#lang racket
(provide (all-defined-out))

(require sicp)
(require "q-eval/repl.rkt")
(require "sample-db.rkt")
(require "q-eval/stream.rkt")

(define repl (make-repl 'display))
(insert-sample-data)

; main
; delayを使用しないと、 flatten-streamはstreamをすべて評価してしまい、計算の遅延ができなくなる
; これはinterleaveの引数である、(stream-car stream)と(flatten-stream (stream-cdr stream))が即座に評価されるからである。
; 計算の遅延ができなくなることで、例えばクエリの結果が返ってくるのが遅くなる。

; 例えば、flatten-streamはstream-flatmapで使用されており、stream-flatmapはsimple-queryで使用されている。
; 以下では、簡単なクエリを実行している。
; わかりやすさのために、flatten-streamの実行のたびに1秒のsleepを組み込んである。また、結果とともに時刻も表示している。

(define start 0)
(define end 0)


(reset-display-time)
(set! start (current-inexact-milliseconds))

(repl '(supervisor ?person (Bitdiddle Ben)))

(set! end (current-inexact-milliseconds))
(display "クエリの実行時間: ")
(display (- end start))
(displayln "ms")

; flatten-streamのdelayを無効にする
(disable-delay)

(reset-display-time)
(set! start (current-inexact-milliseconds))

(repl '(supervisor ?person (Bitdiddle Ben)))

(set! end (current-inexact-milliseconds))
(display "クエリの実行時間: ")
(display (- end start))
(displayln "ms")

; 結果は以下のようになる

; [7003.0ms] {supervisor {Tweakit Lem E} {Bitdiddle Ben}}
; [9004.0ms] {supervisor {Fect Cy D} {Bitdiddle Ben}}
; [10005.0ms] {supervisor {Hacker Alyssa P} {Bitdiddle Ben}}
; 'done
; クエリの実行時間: 12006.792236328125ms

; [11005.0ms] {supervisor {Tweakit Lem E} {Bitdiddle Ben}}
; [11005.0ms] {supervisor {Fect Cy D} {Bitdiddle Ben}}
; [11005.0ms] {supervisor {Hacker Alyssa P} {Bitdiddle Ben}}
; 'done
; クエリの実行時間: 12006.51025390625ms

; delayの有無によってクエリ全体の実行時間は変わらない。つまり、flatten-streamの実行回数に変化はない。
; しかし、delayを使用した方はストリームの評価（flatten-streamの呼び出し）が遅延され、
; 表示に必要なタイミングで必要な評価のみが実行されるため、より早いタイミングで結果が表示されている
; 一方、delayを使用しないと、最初にすべてのストリームを評価するため、表示が遅くなる。

; 今回は明示的にsleepを組み込むことによってこれらの効果をわかりやすくした。
; 実際には、DBにアサーションが非常に多く登録されている場合などで同じ現象が発生する。
