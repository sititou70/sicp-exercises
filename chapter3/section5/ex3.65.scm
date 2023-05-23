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
  (partial-sums s)
  (add-streams s (cons-stream 0 (partial-sums s)))
)

(define 
  (stream-ref s n)
  (if (= n 0) 
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))
  )
)

(define 
  (euler-transform s)
  (define 
    (square x)
    (* x x)
  )
  (let 
    ((s0 (stream-ref s 0)) 
      (s1 (stream-ref s 1))
      (s2 (stream-ref s 2))
    )
    (cons-stream 
      (- s2 
         (/ 
           (square (- s2 s1))
           (+ s0 (* -2 s1) s2)
         )
      )
      (euler-transform (stream-cdr s))
    )
  )
)

(define 
  (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s)))
)
(define 
  (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s))
)

(define 
  (print-stream-nth s n)
  (define 
    (iter s cnt)
    (displayln (stream-car s))
    (if (< cnt (- n 1)) 
      (iter (stream-cdr s) (+ cnt 1))
      'done
    )
  )
  (iter s 0)
)

; main
(define 
  (ln2-summands)
  (define 
    (iter n)
    (cons-stream 
      (* (if (= (remainder n 2) 0) -1 1) (/ 1.0 n))
      (iter (+ n 1))
    )
  )
  (iter 1)
)
(define 
  ln2-stream
  (partial-sums (ln2-summands))
)

(define ln2-euler-transformed-stream (euler-transform ln2-stream))

(define ln2-accelerated-stream (accelerated-sequence euler-transform ln2-stream))

(print-stream-nth ln2-stream 10)
; 1.0
; 0.5
; 0.8333333333333333
; 0.5833333333333333
; 0.7833333333333332
; 0.6166666666666666
; 0.7595238095238095
; 0.6345238095238095
; 0.7456349206349207
; 0.6456349206349207
; 0.69314718055994530941... ; 真の値

(print-stream-nth ln2-euler-transformed-stream 10)
; 0.7
; 0.6904761904761905
; 0.6944444444444444
; 0.6924242424242424
; 0.6935897435897436
; 0.6928571428571428
; 0.6933473389355742
; 0.6930033416875522
; 0.6932539682539683
; 0.6930657506744464
; 0.69314718055994530941... ; 真の値

(print-stream-nth ln2-accelerated-stream 10)
; 1.0
; 0.7
; 0.6932773109243697
; 0.6931488693329254
; 0.6931471960735491
; 0.6931471806635636
; 0.6931471805604039
; 0.6931471805599445
; 0.6931471805599427
; 0.6931471805599454
; 0.69314718055994530941... ; 真の値

; 小数点以下n桁を求めるために計算する必要のある要素数を比べてみる
(define 
  (stream-limit-count s tolerance)
  (define 
    (iter s value n)
    (let 
      ( ;
       (new-value (stream-car s))
      )

      (if (< (abs (- value new-value)) tolerance) 
        n
        (iter (stream-cdr s) new-value (+ n 1))
      )
    )
  )
  (iter (stream-cdr s) (stream-car s) 1)
)

; 3桁
(define tolerance 0.001)
(stream-limit-count ln2-stream tolerance)
; 1000
(stream-limit-count ln2-euler-transformed-stream tolerance)
; 5
(stream-limit-count ln2-accelerated-stream tolerance)
; 3

; 8桁
(define tolerance2 0.00000001)
; (stream-limit-count ln2-stream tolerance2)
; 時間がかかるため実行しない
(stream-limit-count ln2-euler-transformed-stream tolerance2)
; 291
(stream-limit-count ln2-accelerated-stream tolerance2)
; 6

; 14桁
(define tolerance3 0.00000000000001)
; (stream-limit-count ln2-stream tolerance3)
; 時間がかかるため実行しない
; (stream-limit-count ln2-euler-transformed-stream tolerance3)
; 時間がかかるため実行しない
(stream-limit-count ln2-accelerated-stream tolerance3)
; 8
