#lang racket

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
      (cons head (lambda () tail))
    )
  )
)
(define 
  (stream-car stream)
  (car stream)
)
(define 
  (stream-cdr stream)
  ((cdr stream))
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

; main
; 0〜n番目のフィボナッチ数（以降fibと表記）を求めるまでの加算回数をT(n)とおく。すると次のように表される。
; T(0) = 0 ; fibsの定義より、加算は行われない
; T(1) = 0 ; fibsの定義より、加算は行われない
; T(n) = T(n - 1) ; 1つ前までの加算回数
;        + (T(n - 1) - T(n - 2)) ; n - 1番目のfib（のみ）を求めるための加算回数。T(n - 2)を減算することで、T(n - 1)に含まれる「T(n - 2)までの加算回数（1行前の項）」を相殺している。
;        + (T(n - 2) - T(n - 3)) ; n - 2番目のfibを求めるための加算回数。
;        + 1 ; 先程求めた1つ前のfibと2つ前のfibを加算するため。
; 整理すると
; T(n) = 0 (n = 0)
;        0 (n = 1)
;        2T(n - 1) - T(n - 3) + 1 (otherwise)
; となる。
; 具体的に計算すると次のような値になる。
(define 
  (T n)
  (cond 
    ((< n 0) 0)
    ((= n 0) 0)
    ((= n 1) 0)
    (else
     (+ 
       (* (T (- n 1)) 2)
       (* (T (- n 3)) -1)
       1
     )
    )
  )
)
(define 
  (display-t max-n)

  (define 
    (iter n)

    (display "T(")
    (display n)
    (display ")=")
    (displayln (T n))

    (if (< n max-n) 
      (iter (+ n 1))
      'done
    )
  )

  (iter 0)
)
(display-t 20)
; T(0)=0
; T(1)=0
; T(2)=1
; T(3)=3
; T(4)=7
; T(5)=14
; T(6)=26
; T(7)=46
; T(8)=79
; T(9)=133
; T(10)=221
; T(11)=364
; T(12)=596
; T(13)=972
; T(14)=1581
; T(15)=2567
; T(16)=4163
; T(17)=6746
; T(18)=10926
; T(19)=17690
; T(20)=28635

; この計算結果が正しいことを確かめるために、実際にdelay手続きの実装を変更した環境で、加算回数をカウントして表示する。
(define add-count 0)
(define 
  (add-streams s1 s2)
  (stream-map 
    (lambda (x y) 
      (set! add-count (+ add-count 1))
      (+ x y)
    )
    s1
    s2
  )
)
(define 
  fibs
  (cons-stream 
    0
    (cons-stream 1 (add-streams (stream-cdr fibs) fibs))
  )
)
(define 
  (display-add-count max-n)

  (define 
    (iter s n)

    (display "n=")
    (display n)
    (display ", add-count=")
    (displayln add-count)

    (if (< n max-n) 
      (iter (stream-cdr s) (+ n 1))
      'done
    )
  )

  (iter fibs 0)
)
(display-add-count 20)
; n=0, add-count=0
; n=1, add-count=0
; n=2, add-count=1
; n=3, add-count=3
; n=4, add-count=7
; n=5, add-count=14
; n=6, add-count=26
; n=7, add-count=46
; n=8, add-count=79
; n=9, add-count=133
; n=10, add-count=221
; n=11, add-count=364
; n=12, add-count=596
; n=13, add-count=972
; n=14, add-count=1581
; n=15, add-count=2567
; n=16, add-count=4163
; n=17, add-count=6746
; n=18, add-count=10926
; n=19, add-count=17690
; n=20, add-count=28635
; nが20までの実際の加算回数がTに一致することが確かめられた。

; Tの一般項を求めることは難しいが、Tは次の隣接4項間漸化式Uによって下から抑えられる。
; U(n) = 0 (n = 0)
;        0 (n = 1)
;        1 (n = 2)
;        2U(n - 1) - U(n - 3) (otherwise) ; 「+ 1」の項を消した
; 隣接4項間漸化式の一般項は、nの指数の和の形式で表されることが知られている。
; 参考：https://nwuss.nara-wu.ac.jp/media/sites/11/ssh17_05.pdf

; したがって、加算回数はnに対して指数的に増加する。
