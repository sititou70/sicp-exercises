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
  (stream-map proc s)
  (if (stream-null? s) 
    the-empty-stream
    (cons-stream 
      (proc (stream-car s))
      (stream-map proc (stream-cdr s))
    )
  )
)

(define 
  (stream-ref s n)
  (if (= n 0) 
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))
  )
)

; main
; louisのsqrt-streamでは、sqrt-streamは手続きである。したがって、ストリーム内でsqrt-streamを呼び出すたびにcons-streamが実行される。
; 当然、その中で使用されるmemo-procもまた毎回新しく実行されるため、メモが参照されることはない。
; 一方、オリジナルのsqrt-streamではguessesはdefineによって定義される。
; したがって、cons-streamが実行されるのはsqrt-streamを最初に呼び出したときだけであり、メモ化が効果を発揮する。
; memo-procが無効な場合では、両者とも同じだけの効率になる。

; まず、memo-procによる最適化が有効な環境で効率を見積もる。見積もりには、sqrt-improveの実行回数を用いる。
(define improve-count 0)
(define (average x y) (/ (+ x y) 2))
(define 
  (sqrt-improve guess x)
  (set! improve-count (+ improve-count 1))
  (average guess (/ x guess))
)
(define 
  (sqrt-stream x)
  (define 
    guesses
    (cons-stream 
      1.0
      (stream-map 
        (lambda (guess) (sqrt-improve guess x))
        guesses
      )
    )
  )
  guesses
)
(define 
  (sqrt-stream-louis x)
  (cons-stream 
    1.0
    (stream-map 
      (lambda (guess) 
        (sqrt-improve guess x)
      )
      (sqrt-stream-louis x)
    )
  )
)

(set! improve-count 0)
(stream-ref (sqrt-stream 2) 100)
; 1.414213562373095
(displayln improve-count)
; 100

(set! improve-count 0)
(stream-ref (sqrt-stream-louis 2) 100)
; 1.414213562373095
(displayln improve-count)
; 5050

; 次に、メモ化を行わない場合の効率を比較する
(define-syntax 
  cons-stream-no-memo
  (syntax-rules 
    ()
    ((cons-stream head tail) 
      (cons head (lambda () tail))
    )
  )
)
(define 
  (stream-cdr-no-memo stream)
  ((cdr stream))
)
(define 
  (stream-map-no-memo proc s)
  (if (stream-null? s) 
    the-empty-stream
    (cons-stream-no-memo 
      (proc (stream-car s))
      (stream-map-no-memo proc (stream-cdr-no-memo s))
    )
  )
)
(define 
  (stream-ref-no-memo s n)
  (if (= n 0) 
    (stream-car s)
    (stream-ref-no-memo (stream-cdr-no-memo s) (- n 1))
  )
)
(define 
  (sqrt-stream-no-memo x)
  (define 
    guesses
    (cons-stream-no-memo 
      1.0
      (stream-map-no-memo 
        (lambda (guess) (sqrt-improve guess x))
        guesses
      )
    )
  )
  guesses
)
(define 
  (sqrt-stream-louis-no-memo x)
  (cons-stream-no-memo 
    1.0
    (stream-map-no-memo 
      (lambda (guess) 
        (sqrt-improve guess x)
      )
      (sqrt-stream-louis-no-memo x)
    )
  )
)

(set! improve-count 0)
(stream-ref-no-memo (sqrt-stream-no-memo 2) 100)
; 1.414213562373095
(displayln improve-count)
; 5050

(set! improve-count 0)
(stream-ref-no-memo (sqrt-stream-louis-no-memo 2) 100)
; 1.414213562373095
(displayln improve-count)
; 5050
