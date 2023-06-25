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

; すべての自然数のペアを次のように3つに分解する。これはex3.60で実装した方法に似ている。

;     1   2   3   4   5   6   7   ...
; 1  (a) (            b            )
; 2  +-----------------------------+
; 3  |                             |
; 4  |                             |
; 5  |              c              |
; 6  |                             |
; .  |                             |
; .  |                             |
; .  +-----------------------------+

; すると、次のような実装を得る
(define 
  (pairs s t)
  (cons-stream 
    (list (stream-car s) (stream-car t)) ; (a)のペア
    (interleave 
      (stream-map 
        (lambda (x) (list (stream-car s) x)) ; (b) のストリーム
        (stream-cdr t)
      )
      (pairs (stream-cdr s) t) ; 残りの部分(c)
    )
  )
)
(print-stream-nth (pairs integers integers) 20)
; [0] (1 1)
; [1] (1 2)
; [2] (2 1)
; [3] (1 3)
; [4] (2 2)
; [5] (1 4)
; [6] (3 1)
; [7] (1 5)
; [8] (2 3)
; [9] (1 6)
; [10] (3 2)
; [11] (1 7)
; [12] (2 4)
; [13] (1 8)
; [14] (4 1)
; [15] (1 9)
; [16] (2 5)
; [17] (1 10)
; [18] (3 3)
; [19] (1 11)

; または、次のように4つに分解して、別の実装も得られる
; 問題文のヒントから、こちらの実装が想定解であると思われる。

;     1   2   3   4   5   6   7   ...
; 1  (a) (            b            )
; 2  +-+ +-------------------------+
; 3  | | |                         |
; 4  | | |                         |
; 5  |c| |            d            |
; 6  | | |                         |
; .  | | |                         |
; .  | | |                         |
; .  +-+ +-------------------------+

(define 
  (pairs2 s t)
  (cons-stream 
    (list (stream-car s) (stream-car t)) ; (a)のペア
    (interleave 
      (interleave 
        (stream-map 
          (lambda (x) (list (stream-car s) x)) ; (b) のストリーム
          (stream-cdr t)
        )
        (stream-map 
          (lambda (x) (list x (stream-car t))) ; (c) のストリーム
          (stream-cdr s)
        )
      )
      (pairs (stream-cdr s) (stream-cdr t)) ; 残りの部分(d)
    )
  )
)
(print-stream-nth (pairs2 integers integers) 20)
; [0] (1 1)
; [1] (1 2)
; [2] (2 2)
; [3] (2 1)
; [4] (2 3)
; [5] (1 3)
; [6] (3 2)
; [7] (3 1)
; [8] (2 4)
; [9] (1 4)
; [10] (3 3)
; [11] (4 1)
; [12] (2 5)
; [13] (1 5)
; [14] (4 2)
; [15] (5 1)
; [16] (2 6)
; [17] (1 6)
; [18] (3 4)
; [19] (6 1)
