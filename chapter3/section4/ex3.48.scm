#lang racket/base
(require sicp)
(require (only-in srfi/1 [iota srfi-iota] [map srfi-map] [for-each srfi-for-each]))

; utils
(define 
  (random-sleep)
  (sleep (/ (random 10) 1000))
)

(define tas-semaphore (make-semaphore 1))
(define 
  (test-and-set! cell)

  (semaphore-wait tas-semaphore)
  (define ret (if (car cell) true (begin (set-car! cell true) false)))
  (semaphore-post tas-semaphore)
  ret
)

(define 
  (make-mutex)

  (define (clear! cell) (set-car! cell false))

  (let 
    ((cell (list false)))
    (define 
      (the-mutex m)
      (cond 
        ((eq? m 'acquire)
         (random-sleep)
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
  (make-serializer)
  (let 
    ((mutex (make-mutex)))
    (lambda (p) 
      (define 
        (serialized-p . args)
        (mutex 'acquire)
        (let 
          ((val (apply p args)))
          (mutex 'release)
          val
        )
      )
      serialized-p
    )
  )
)

; account
(define 
  (make-account-and-serializer id balance)
  (define 
    (withdraw amount)
    (if (>= balance amount) 
      (begin 
        (set! balance (- balance amount))
        balance
      )
      " Insufficient funds "
    )
  )
  (define 
    (deposit amount)
    (set! balance (+ balance amount))
    balance
  )
  (let 
    ((balance-serializer (make-serializer)))
    (define 
      (dispatch m)
      (cond 
        ((eq? m 'withdraw) withdraw)
        ((eq? m 'deposit) deposit)
        ((eq? m 'balance) balance)
        ((eq? m 'serializer) balance-serializer)
        ((eq? m 'id) id)
        (else (error "Unknown request: MAKE-ACCOUNT" m))
      )
    )
    dispatch
  )
)

(define 
  (exchange account1 account2)
  (let 
    ((difference 
       (- (account1 'balance) 
          (account2 'balance)
       )
     ) 
    )
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)
  )
)

(define 
  (serialized-exchange account1 account2)
  (let 
    ( ;
     (serializer1 (account1 'serializer))
     (serializer2 (account2 'serializer))
     (id1 (account1 'id))
     (id2 (account2 'id))
    )
    (if (< id1 id2) 
      ((serializer1 (serializer2 exchange)) 
        account1
        account2
      )
      ((serializer2 (serializer1 exchange)) 
        account1
        account2
      )
    )
  )
)

; main
; 以下のIDをすべて同じにするとデッドロックの可能性があるようにできる
(define account1 (make-account-and-serializer 1 10))
(define account2 (make-account-and-serializer 2 20))
(define account3 (make-account-and-serializer 3 30))
(define 
  exchanges
  (list 
    (list account1 account2)
    (list account1 account3)
    (list account2 account1)
    (list account2 account3)
    (list account3 account1)
    (list account3 account2)
  )
)

(define 
  (random-exchange)
  (apply 
    serialized-exchange
    (list-ref exchanges (random (length exchanges)))
  )
)
(define 
  threads
  (srfi-map 
    (lambda (_) 
      (thread random-exchange)
    )
    (srfi-iota 100)
  )
)
(srfi-for-each thread-wait threads)

(display "account1: ")
(displayln (account1 'balance))
(display "account2: ")
(displayln (account2 'balance))
(display "account3: ")
(displayln (account3 'balance))

; 一般的に、デッドロックは資源割付グラフにループがあることによって発生する。
; 資源割付グラフのノードは、プロセスノード（P）と資源ノード（R）の2種類である。
; プロセスノードから資源ノードへのエッジは、プロセスが資源のロックを要求していることを表す。
; 資源ノードからプロセスノードへのエッジは、資源がプロセスによってロックされていることを表す。
; ここで、一般にn個のプロセスと資源で発生するデッドロックは以下のように表される。

; P1 -> R1 -> P2 -> R2 -> ... PN -> RN
; ^                                 |
; |                                 |
; +---------------------------------+

; ここで、各プロセスが資源番号の小さい順からロックを取得しようとすることで、デッドロックを回避できる。
; なぜなら、プロセス1〜NがリソースNのロックを先に取得することは無くなるため、資源割付グラフでループが発生し得ないからである。
