#lang racket

; from: https://stackoverflow.com/questions/13998388/running-code-from-sicp-section-3-5-4-with-drracket
(define the-empty-stream '())
(define 
  (stream-null? stream)
  (null? stream)
)
(define-syntax 
  cons-stream
  (syntax-rules 
    ()
    ((cons-stream head tail) 
      (cons head (delay tail))
    )
  )
)
(define 
  (stream-car stream)
  (car stream)
)
(define 
  (stream-cdr stream)
  (force (cdr stream))
)

; utils
(define 
  (stream-map proc . argstreams)
  (if (stream-null? (car argstreams)) 
    the-empty-stream
    (cons-stream 
      (apply proc (map stream-car argstreams))
      (apply stream-map 
             (cons proc (map stream-cdr argstreams))
      )
    )
  )
)

(define 
  (stream-filter pred stream)
  (cond 
    ((stream-null? stream) the-empty-stream)
    ((pred (stream-car stream))
     (cons-stream 
       (stream-car stream)
       (stream-filter 
         pred
         (stream-cdr stream)
       )
     )
    )
    (else (stream-filter pred (stream-cdr stream)))
  )
)

(define (add-streams s1 s2) (stream-map + s1 s2))

(define 
  (scale-stream stream factor)
  (stream-map 
    (lambda (x) (* x factor))
    stream
  )
)

(define 
  (stream-ref s n)
  (if (= n 0) 
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))
  )
)

(define 
  (print-stream-nth s n)
  (define 
    (iter s cnt)
    (display "[")
    (display cnt)
    (display "] ")
    (displayln (stream-car s))
    (if (< cnt (- n 1)) 
      (iter (stream-cdr s) (+ cnt 1))
      'done
    )
  )
  (iter s 0)
)

; main
; ここでは、以下のような回路において、時刻0にスイッチをONにした場合を考える

;     R[Ω]     C[F]
; +---^v^v^v---||-----------+
; |                         |
; +---./.---[+ battery -]---+
;     SW       E[V]

(define 
  (integral integrand initial-value dt)
  (define 
    int
    (cons-stream 
      initial-value
      (add-streams 
        (scale-stream integrand dt)
        int
      )
    )
  )
  int
)

(define 
  (RC R C dt)

  (define 
    (v i-stream v0)
    (add-streams 
      (scale-stream i-stream R)
      (integral 
        (scale-stream i-stream (/ 1.0 C))
        v0
        dt
      )
    )
  )
  v
)

; 回路を流れる電流は、(E/R) * e^(-t/(CR))で表される
; 参考：https://detail-infomation.com/transient-rc-series-circuit-differential-equation/
(define 
  (RC-i E R C dt)
  (define 
    (i t)
    (* (/ E R) 
       (exp 
         (* -1 
            (/ 
              1.0
              (* C R)
            )
            t
         )
       )
    )
  )
  (define 
    (iter t)
    (cons-stream (i t) (iter (+ t dt)))
  )
  (iter 0)
)

(define E 9.0)
(define R 5.0)
(define C 1.0)
(define dt 0.5)
(define RC1 (RC R C dt))
(define RC1-i (RC-i E R C dt))

; 10秒後までの回路にかかる電圧の変化は
(print-stream-nth (RC1 RC1-i 0) (/ 10 dt))
; [0] 9.0
; [1] 9.043536762323635
; [2] 9.0829304539342
; [3] 9.118575340138008
; [4] 9.150828166936847
; [5] 9.180011731461871
; [6] 9.206418112635776
; [7] 9.230311594396849
; [8] 9.251931310741424
; [9] 9.271493639057322
; [10] 9.289194365701452
; [11] 9.305210645495483
; [12] 9.319702774750857
; [13] 9.332815795568134
; [14] 9.34468094746709
; [15] 9.355416980875946
; [16] 9.365131345625564
; [17] 9.373921266343466
; [18] 9.381874715510595
; [19] 9.389071293919459
; 時刻によらず、Eで一定である。
; 計算結果では微妙に電圧が上昇しているように見えるが、積分処理の誤差である。
; 曲線的に変化する電流を、dtを幅とする矩形で積分するため誤差が発生している。
; これはdtを小さくすることで真の値Eに近づく。

; 実際にさらに小さいdt2を設定して
(define dt2 0.00005)
(define RC1-detailed (RC R C dt2))
(define RC1-i-detailed (RC-i E R C dt2))
; 先程と同じく10秒後の電圧を計算すると
(stream-ref (RC1-detailed RC1-i-detailed 0) (/ 10 dt2))
; 9.000038909985648
; よりEに近い結果が得られる。

; また、RC回路では、時定数CR[s]の時刻でコンデンサにかかる電圧は、E(1 - (1/e))となることが知られている。
; 実際に計算してみると
(define 
  (RC-Cv C dt)

  (define 
    (v i-stream v0)
    (integral 
      (scale-stream i-stream (/ 1.0 C))
      v0
      dt
    )
  )
  v
)
(define RC1-Cv-detailed (RC-Cv C dt2))
(stream-ref 
  (RC1-Cv-detailed RC1-i-detailed 0)
  (/ (* C R) dt2)
)
; 5.689113474930025
(* E (- 1 (exp -1)))
; 5.6890850294570185
; 確かに近い値が得られている
