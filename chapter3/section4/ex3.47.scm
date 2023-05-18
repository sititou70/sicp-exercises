#lang racket/base
(require sicp)
(require (only-in srfi/1 [iota srfi-iota] [map srfi-map] [for-each srfi-for-each]))

; test-and-set!ユーティリティ
(define tas-semaphore (make-semaphore 1))
(define 
  (test-and-set! cell)

  (semaphore-wait tas-semaphore)
  (define ret (if (car cell) true (begin (set-car! cell true) false)))
  (semaphore-post tas-semaphore)
  ret
)

; a. ミューテックスを使ったセマフォの実装
(define 
  (make-mutex)

  (define (clear! cell) (set-car! cell false))

  (let 
    ((cell (list false)))
    (define 
      (the-mutex m)
      (cond 
        ((eq? m 'acquire)
         (if (test-and-set! cell) 
           (the-mutex 'acquire) ; retry
         )
        )
        ((eq? m 'release) (clear! cell))
      )
    )
    the-mutex
  )
)

(define 
  (make-semaphore-a n)
  (define counter 0)
  (define mutex (make-mutex))

  (define 
    (the-semaphore m)
    (cond 
      ((eq? m 'acquire)
       (mutex 'acquire)
       (if (< counter n) 
         (begin 
           (set! counter (+ counter 1))
           (mutex 'release)
         )
         (begin 
           (mutex 'release)
           (the-semaphore m) ; retry
         )
       )
      )
      ((eq? m 'release)
       (mutex 'acquire)
       (set! counter (- counter 1))
       (mutex 'release)
      )
    )
  )

  the-semaphore
)

; b. アトミックな test-and-set! 演算を用いるセマフォの実装
(define 
  (make-semaphore-b n)
  (define counter 0)
  (define mutex-cell (list false))

  (define 
    (the-semaphore m)
    (cond 
      ((eq? m 'acquire)
       (if (test-and-set! mutex-cell) (the-semaphore m)) ; acquire
       (if (< counter n) 
         (begin 
           (set! counter (+ counter 1))
           (set-car! mutex-cell false) ; release
         )
         (begin 
           (set-car! mutex-cell false) ; release
           (the-semaphore m) ; retry
         )
       )
      )
      ((eq? m 'release)
       (if (test-and-set! mutex-cell) (the-semaphore m)) ; acquire
       (set! counter (- counter 1))
       (set-car! mutex-cell false) ; release
      )
    )
  )

  the-semaphore
)

; main
;; セマフォがない場合：ランダムなリソースが表示される
(define resource 0)
(define 
  (inc-dec)

  (define 
    (sleep-random)
    (sleep (/ (random 10) 1000))
  )

  (sleep-random)
  (set! resource (+ resource 1))
  (sleep-random)
  (display resource)
  (display ", ")
  (sleep-random)
  (set! resource (- resource 1))
  (sleep-random)
)
(define 
  threads
  (srfi-map 
    (lambda (_) 
      (thread inc-dec)
    )
    (srfi-iota 100)
  )
)
(srfi-for-each thread-wait threads)
(newline)
; 4, 12, 14, 21, 22, 26, 27, 29, 31, 35, 44, 44, 44, 44, 46, 48, 48, 49, 49, 50, 50, 52, 54, 54, 56, 56, 59, 58, 61, 62, 61, 61, 62, 62, 63, 64, 64, 64, 65, 64, 64, 67, 68, 71, 71, 71, 73, 77, 76, 77, 77, 77, 79, 78, 78, 78, 78, 78, 75, 74, 74, 74, 72, 72, 71, 71, 69, 69, 64, 63, 63, 61, 57, 56, 54, 53, 46, 46, 46, 45, 45, 44, 44, 44, 42, 42, 41, 39, 38, 36, 33, 31, 31, 29, 27, 27, 25, 23, 23, 23, 

;; aの5までのセマフォを使った場合：5までのリソースが表示される
(define resource-a 0)
(define semaphore-a (make-semaphore-a 5))
(define 
  (inc-dec-a)

  (define 
    (sleep-random)
    (sleep (/ (random 10) 1000))
  )

  (semaphore-a 'acquire)
  (sleep-random)
  (set! resource (+ resource 1))
  (sleep-random)
  (display resource)
  (display ", ")
  (sleep-random)
  (set! resource (- resource 1))
  (sleep-random)
  (semaphore-a 'release)
)
(define 
  threads-a
  (srfi-map 
    (lambda (_) 
      (thread inc-dec-a)
    )
    (srfi-iota 100)
  )
)
(srfi-for-each thread-wait threads-a)
(newline)
; 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3, 3, 1, 

;; bの5までのセマフォを使った場合：5までのリソースが表示される
(define resource-b 0)
(define semaphore-b (make-semaphore-b 5))
(define 
  (inc-dec-b)

  (define 
    (sleep-random)
    (sleep (/ (random 10) 1000))
  )

  (semaphore-b 'acquire)
  (sleep-random)
  (set! resource (+ resource 1))
  (sleep-random)
  (display resource)
  (display ", ")
  (sleep-random)
  (set! resource (- resource 1))
  (sleep-random)
  (semaphore-b 'release)
)
(define 
  threads-b
  (srfi-map 
    (lambda (_) 
      (thread inc-dec-b)
    )
    (srfi-iota 100)
  )
)
(srfi-for-each thread-wait threads-b)
; 5, 5, 5, 5, 5, 4, 4, 4, 4, 3, 2, 3, 3, 4, 3, 4, 2, 2, 2, 2, 3, 3, 2, 2, 4, 3, 3, 3, 3, 2, 3, 3, 4, 4, 1, 2, 3, 5, 5, 5, 2, 2, 2, 2, 5, 4, 4, 4, 3, 2, 2, 3, 2, 2, 4, 4, 4, 4, 1, 3, 4, 4, 4, 1, 3, 3, 3, 1, 3, 3, 4, 3, 1, 3, 4, 4, 4, 3, 4, 4, 4, 3, 2, 2, 3, 2, 4, 5, 5, 5, 4, 3, 2, 2, 3, 4, 3, 2, 2, 1, %                                                                                                 
