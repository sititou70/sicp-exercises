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

(define (add-streams s1 s2) (stream-map + s1 s2))

(define 
  (interleave s1 s2)
  (if (stream-null? s1) 
    s2
    (cons-stream 
      (stream-car s1)
      (interleave s2 (stream-cdr s1))
    )
  )
)

(define 
  (pairs s t)
  (cons-stream 
    (list (stream-car s) (stream-car t))
    (interleave 
      (stream-map 
        (lambda (x) (list (stream-car s) x))
        (stream-cdr t)
      )
      (pairs (stream-cdr s) (stream-cdr t))
    )
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
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define integer-pairs (pairs integers integers))

; integer-pairsは次のようになる
(print-stream-nth integer-pairs 20)
; [0] (1 1)
; [1] (1 2)
; [2] (2 2)
; [3] (1 3)
; [4] (2 3)
; [5] (1 4)
; [6] (3 3)
; [7] (1 5)
; [8] (2 4)
; [9] (1 6)
; [10] (3 4)
; [11] (1 7)
; [12] (2 5)
; [13] (1 8)
; [14] (4 4)
; [15] (1 9)
; [16] (2 6)
; [17] (1 10)
; [18] (3 5)
; [19] (1 11)

; integer-pairsが生成するペアの1つ目を縦軸i、2つ目を横軸jとして、その添字（ストリーム内でそのペア現れる位置）を表にする。添字は0から始める。

;     1   2   3   4   5   6   7   8   9       j
; 1   0   1   3   5   7   9   11  13  15
; 2       2   4   8   12  16  20  24  28
; 3           6   10  18  26  34  42  50
; 4               14  22  38  54  70  86
; 5                   30  46  78  110 142
; 
; i

; i = jのペアに関しては、2^i - 2になっているとわかる。

; i != jのペアに関しては横方向への等差数列になっているとわかる
; 初項は、1つ左（i = j）の添字 + 2^(i - 1)であるため、2^i - 2 + 2^(i - 1)
; 公差は、2^i
; 等差数列の添字（0から始まる）は、j - i - 1である。
; したがってペア(i, j)の添字は、2^i - 2 + 2^(i - 1) + 2^i * (j - i - 1)
; 整理すると、2^(i - 1) * (2(j - i) + 1) - 2

; したがって、ペア(i, j)の前に現れる要素数（＝integer-pairsにおける0から始まる添字）をI(i, j)とすると、以下のようになる。
; I(i, j) = 2^i - 2 (i = j)
;         = 2^(i - 1) * (2(j - i) + 1) - 2 (otherwise)

; integer-pairsが生成する1,000,000個目までのペアに対して上記の式を検算する
(define 
  (I pair)
  (let 
    (
     ;
     (i (car pair))
     (j (cadr pair))
    )

    (if (= i j) 
      (- (expt 2 i) 2)
      (- 
        (* 
          (expt 2 (- i 1))
          (+ (* 2 (- j i)) 1)
        )
        2
      )
    )
  )
)
(define 
  (check max-n)
  (define 
    (iter s n)
    (cond 
      ((= n max-n) 'allok)
      ((= n (I (stream-car s)))
       (iter (stream-cdr s) (+ n 1))
      )
      (else 'ng)
    )
  )
  (iter integer-pairs 0)
)
(check 1000000)
; 'allok

; したがって、問題文における各要素の前には、それぞれ次のような数のペアが出てくる
(I (list 1 100))
; 197
(I (list 99 100))
; 950737950171172051122527404030
(I (list 100 100))
; 1267650600228229401496703205374

; 表を描くアイデアは次のサイトを参考にした：http://community.schemewiki.org/?sicp-ex-3.66
