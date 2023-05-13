#lang racket/base
(require sicp)

; queue
(define (make-queue) (cons '() '()))
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define 
  (set-front-ptr! queue item)
  (set-car! queue item)
)
(define 
  (set-rear-ptr! queue item)
  (set-cdr! queue item)
)

(define 
  (empty-queue? queue)
  (null? (front-ptr queue))
)
(define 
  (front-queue queue)
  (if (empty-queue? queue) 
    (error "FRONT called with an empty queue " queue)
    (car (front-ptr queue))
  )
)

(define 
  (insert-queue! queue item)
  (let 
    ((new-pair (cons item ' ())))
    (cond 
      ((empty-queue? queue)
       (set-front-ptr! queue new-pair)
       (set-rear-ptr! queue new-pair)
       queue
      )
      (else
       (set-cdr! (rear-ptr queue) new-pair)
       (set-rear-ptr! queue new-pair)
       queue
      )
    )
  )
)
(define 
  (delete-queue! queue)
  (cond 
    ((empty-queue? queue)
     (error "DELETE! called with an empty queue" queue)
    )
    (else
     (set-front-ptr! queue (cdr (front-ptr queue)))
     queue
    )
  )
)

; 回路
(define 
  (make-wire)

  (define 
    (call-each procedures)
    (if (null? procedures) 
      'done
      (begin 
        ((car procedures))
        (call-each (cdr procedures))
      )
    )
  )

  (let 
    ( ;
     (signal-value 0)
     (action-procedures '())
    )

    (define 
      (set-my-signal! new-value)
      (if (not (= signal-value new-value)) 
        (begin 
          (set! signal-value new-value)
          (call-each action-procedures)
        )
        'done
      )
    )

    (define 
      (accept-action-procedure! proc)
      (set! 
        action-procedures
        (cons proc action-procedures)
      )
      (proc)
    )

    (define 
      (dispatch m)
      (cond 
        ((eq? m 'get-signal) signal-value)
        ((eq? m 'set-signal!) set-my-signal!)
        ((eq? m 'add-action!) accept-action-procedure!)
        (else (error " Unknown operation : WIRE" m))
      )
    )

    dispatch
  )
)

(define (get-signal wire) (wire 'get-signal))
(define 
  (set-signal! wire new-value)
  ((wire 'set-signal!) new-value)
)
(define 
  (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure)
)

; 予定表
(define 
  (make-time-segment time queue)
  (cons time queue)
)
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define 
  (set-current-time! agenda time)
  (set-car! agenda time)
)
(define (segments agenda) (cdr agenda))
(define 
  (set-segments! agenda segments)
  (set-cdr! agenda segments)
)
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define 
  (empty-agenda? agenda)
  (null? (segments agenda))
)

(define 
  (add-to-agenda! time action agenda)

  (define 
    (belongs-before? segments)
    (or (null? segments) 
        (< time (segment-time (car segments)))
    )
  )

  (define 
    (make-new-time-segment time action)
    (let 
      ( ;
       (q (make-queue))
      )

      (insert-queue! q action)
      (make-time-segment time q)
    )
  )

  (define 
    (add-to-segments! segments)
    (if (= (segment-time (car segments)) time) 
      (insert-queue! 
        (segment-queue (car segments))
        action
      )
      (let 
        ((rest (cdr segments)))
        (if (belongs-before? rest) 
          (set-cdr! 
            segments
            (cons (make-new-time-segment time action) 
                  (cdr segments)
            )
          )
          (add-to-segments! rest)
        )
      )
    )
  )

  (let 
    ((segments (segments agenda)))
    (if (belongs-before? segments) 
      (set-segments! 
        agenda
        (cons (make-new-time-segment time action) 
              segments
        )
      )
      (add-to-segments! segments)
    )
  )
)

(define 
  (remove-first-agenda-item! agenda)
  (let 
    ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q) 
      (set-segments! agenda (rest-segments agenda))
    )
  )
)

(define 
  (first-agenda-item agenda)
  (if (empty-agenda? agenda) 
    (error "Agenda is empty : FIRST-AGENDA-ITEM")
    (let 
      ( ;
       (first-seg (first-segment agenda))
      )

      (set-current-time! 
        agenda
        (segment-time first-seg)
      )
      (front-queue (segment-queue first-seg))
    )
  )
)

; 基本関数箱
(define 
  (after-delay delay action)
  (add-to-agenda! 
    (+ delay (current-time the-agenda))
    action
    the-agenda
  )
)

(define 
  (inverter input output)

  (define 
    (logical-not s)
    (cond 
      ((= s 0) 1)
      ((= s 1) 0)
      (else (error "Invalid signal" s))
    )
  )

  (define 
    (invert-input)
    (let 
      ((new-value (logical-not (get-signal input))))
      (after-delay 
        inverter-delay
        (lambda () (set-signal! output new-value))
      )
    )
  )

  (add-action! input invert-input)
  'ok
)

(define 
  (and-gate a1 a2 output)

  (define 
    (logical-and s1 s2)
    (cond 
      ((and (= s1 0) (= s2 0)) 0)
      ((and (= s1 0) (= s2 1)) 0)
      ((and (= s1 1) (= s2 0)) 0)
      ((and (= s1 1) (= s2 1)) 1)
      (else (error "Invalid signal" (list s1 s2)))
    )
  )

  (define 
    (and-action-procedure)
    (let 
      ((new-value 
         (logical-and (get-signal a1) (get-signal a2))
       ) 
      )
      (after-delay 
        and-gate-delay
        (lambda () (set-signal! output new-value))
      )
    )
  )

  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok
)

(define 
  (or-gate a1 a2 output)

  (define 
    (logical-or s1 s2)
    (cond 
      ((and (= s1 0) (= s2 0)) 0)
      ((and (= s1 0) (= s2 1)) 1)
      ((and (= s1 1) (= s2 0)) 1)
      ((and (= s1 1) (= s2 1)) 1)
      (else (error "Invalid signal" (list s1 s2)))
    )
  )

  (define 
    (or-action-procedure)
    (let 
      ((new-value 
         (logical-or (get-signal a1) (get-signal a2))
       ) 
      )
      (after-delay 
        or-gate-delay
        (lambda () (set-signal! output new-value))
      )
    )
  )

  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok
)

; 加算器
(define 
  (half-adder a b s c)
  (let 
    ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok
  )
)

(define 
  (full-adder a b c-in sum c-out)
  (let 
    ( ;
     (s (make-wire))
     (c1 (make-wire))
     (c2 (make-wire))
    )

    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok
  )
)

(define 
  (ripple-carry-adder a-list b-list c-in s-list c-out)

  (let 
    ( ;
     (a (car a-list))
     (b (car b-list))
     (s (car s-list))
     (last? 
       (and 
         (= (length a-list) 1)
         (= (length b-list) 1)
         (= (length s-list) 1)
       )
     )
    )

    (if last? 
      (full-adder a b c-in s c-out)
      (let 
        ( ;
         (current-c-out (make-wire))
        )

        (full-adder a b c-in s current-c-out)
        (ripple-carry-adder 
          (cdr a-list)
          (cdr b-list)
          current-c-out
          (cdr s-list)
          c-out
        )
      )
    )
  )
)

; utils
(define 
  (probe name wire)
  (add-action! 
    wire
    (lambda () 
      (display "[")
      (display (current-time the-agenda))
      (display "]")
      (display " ")

      (display name)
      (display " = ")
      (display (get-signal wire))
      (newline)
    )
  )
)

(define 
  (propagate)
  (if (empty-agenda? the-agenda) 
    'done
    (let 
      ((first-item (first-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate)
    )
  )
)

; main
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

; ここでは、簡単のために各桁で必ず繰り上がりが発生する計算、つまり1111 + 00001のような計算のみを想定する。
; 繰り上がり全加算器では、各全加算器の繰り上がりビットが全体の計算時間に大きく関係するからである。

; まず、半加算器、全加算器について、入力が時刻0に確定した場合、出力が確定する時刻を求める。
; 半加算器では
(define 
  half-s-time
  (+ (max or-gate-delay (+ and-gate-delay inverter-delay)) and-gate-delay)
)
(define half-c-time and-gate-delay)
half-s-time
; 8
half-c-time
; 3
; となる

; 全加算器では
(define full-sum-time (+ half-s-time half-s-time))
(define 
  full-c-out-time
  (+ (max (+ half-s-time half-c-time) half-c-time) or-gate-delay)
)
full-sum-time
; 16
full-c-out-time
; 16
; となる

; nビットの繰り上がり全加算器では、まず1桁目のsumとc-outの計算に16かかる。
; 次に、繰り上がりビットの変化が次の全加算器のc-inとして入力されるが、ここでsumの計算には16かかるのに対して、c-outの計算には8しかかからない
; c-inからsumを算出するには2つの半加算器のsの算出を必ず待たなければならないのに対して、c-inからc-outを計算するには、半加算器のcとorの計算結果だけで良い。
; その後orのもう一方の値も変化するが、すでにc-outは1であるため、値に変化はない。
; 繰り上がり全加算器の最後の全加算器では、c-outの計算には、直前のc-inの計算にかかる時間 + 8がかかる。sumの計算には、直前のc-inの計算にかかる時間 + 16がかかる。
; このことから、一般に繰り上がり全加算器の計算時間はc-out <= sumである。
; したがって、nビットの繰り上がり全加算器の計算にかかる時間をT(n)とすると
; T(n) = 16 (n = 1)
;        16 + 8(n - 2) + 16 (otherwise)
; となる。特定の場合について
; T(1) = 16
; T(2) = 32
; T(3) = 40
; T(4) = 48
; である。
; 各関数箱の遅延時間を使って表現すると
; T(n) = max(full-sum-time, full-c-out-time) (n = 1)
;        full-c-out-time + (half-c-time + or-gate-delay)(n - 2) + max(2 * half-s-time, half-c-time + or-gate-delay)) (otherwise)
; となる。

(newline)
(newline)
(display "シミュレーション")
(newline)

(newline)
(newline)
(display "1bit")
(newline)

(define a-list-1bit (list (make-wire)))
(define b-list-1bit (list (make-wire)))
(define c-in-1bit (make-wire))
(define s-list-1bit (list (make-wire)))
(define c-out-1bit (make-wire))
(ripple-carry-adder a-list-1bit b-list-1bit c-in-1bit s-list-1bit c-out-1bit)

; 初期状態が安定するまで待つ
(propagate)
(probe 's-list-1bit-0 (car s-list-1bit))
(probe 'c-out-1bit c-out-1bit)
; [5] s-list-1bit-0 = 0
; [5] c-out-1bit = 0

(set-signal! (car a-list-1bit) 1)
(set-signal! (car b-list-1bit) 1)
(propagate)
; [13] s-list-1bit-0 = 1
; [21] c-out-1bit = 1
; [21] s-list-1bit-0 = 0
; 計算時間は16 = 21 - 5

(newline)
(newline)
(display "2bit")
(newline)

(define a-list-2bit (list (make-wire) (make-wire)))
(define b-list-2bit (list (make-wire) (make-wire)))
(define c-in-2bit (make-wire))
(define s-list-2bit (list (make-wire) (make-wire)))
(define c-out-2bit (make-wire))
(ripple-carry-adder a-list-2bit b-list-2bit c-in-2bit s-list-2bit c-out-2bit)

; 初期状態が安定するまで待つ
(propagate)
(probe 's-list-2bit-0 (car s-list-2bit))
(probe 's-list-2bit-1 (cadr s-list-2bit))
(probe 'c-out-2bit c-out-2bit)
; [26] s-list-2bit-0 = 0
; [26] s-list-2bit-1 = 0
; [26] c-out-2bit = 0

(set-signal! (car a-list-2bit) 1)
(set-signal! (car b-list-2bit) 1)
(set-signal! (cadr b-list-2bit) 1)
(propagate)
; [34] s-list-2bit-0 = 1
; [42] s-list-2bit-1 = 1
; [42] s-list-2bit-0 = 0
; [50] c-out-2bit = 1
; [58] s-list-2bit-1 = 0
; 計算時間は32 = 58 - 26

(newline)
(newline)
(display "3bit")
(newline)

(define a-list-3bit (list (make-wire) (make-wire) (make-wire)))
(define b-list-3bit (list (make-wire) (make-wire) (make-wire)))
(define c-in-3bit (make-wire))
(define s-list-3bit (list (make-wire) (make-wire) (make-wire)))
(define c-out-3bit (make-wire))
(ripple-carry-adder a-list-3bit b-list-3bit c-in-3bit s-list-3bit c-out-3bit)

; 初期状態が安定するまで待つ
(propagate)
(probe 's-list-3bit-0 (car s-list-3bit))
(probe 's-list-3bit-1 (cadr s-list-3bit))
(probe 's-list-3bit-2 (caddr s-list-3bit))
(probe 'c-out-3bit c-out-3bit)
; [63] s-list-3bit-0 = 0
; [63] s-list-3bit-1 = 0
; [63] s-list-3bit-2 = 0
; [63] c-out-3bit = 0

(set-signal! (car a-list-3bit) 1)
(set-signal! (car b-list-3bit) 1)
(set-signal! (cadr b-list-3bit) 1)
(set-signal! (caddr b-list-3bit) 1)
(propagate)
; [71] s-list-3bit-0 = 1
; [79] s-list-3bit-1 = 1
; [79] s-list-3bit-2 = 1
; [79] s-list-3bit-0 = 0
; [95] c-out-3bit = 1
; [95] s-list-3bit-1 = 0
; [103] s-list-3bit-2 = 0
; 計算時間は40 = 103 - 63

(newline)
(newline)
(display "4bit")
(newline)

(define a-list-4bit (list (make-wire) (make-wire) (make-wire) (make-wire)))
(define b-list-4bit (list (make-wire) (make-wire) (make-wire) (make-wire)))
(define c-in-4bit (make-wire))
(define s-list-4bit (list (make-wire) (make-wire) (make-wire) (make-wire)))
(define c-out-4bit (make-wire))
(ripple-carry-adder a-list-4bit b-list-4bit c-in-4bit s-list-4bit c-out-4bit)

; 初期状態が安定するまで待つ
(propagate)
(probe 's-list-4bit-0 (car s-list-4bit))
(probe 's-list-4bit-1 (cadr s-list-4bit))
(probe 's-list-4bit-2 (caddr s-list-4bit))
(probe 's-list-4bit-3 (cadddr s-list-4bit))
(probe 'c-out-4bit c-out-4bit)
; [108] s-list-4bit-0 = 0
; [108] s-list-4bit-1 = 0
; [108] s-list-4bit-2 = 0
; [108] s-list-4bit-3 = 0
; [108] c-out-4bit = 0

(set-signal! (car a-list-4bit) 1)
(set-signal! (car b-list-4bit) 1)
(set-signal! (cadr b-list-4bit) 1)
(set-signal! (caddr b-list-4bit) 1)
(set-signal! (cadddr b-list-4bit) 1)
(propagate)
; [116] s-list-4bit-0 = 1
; [124] s-list-4bit-1 = 1
; [124] s-list-4bit-2 = 1
; [124] s-list-4bit-3 = 1
; [124] s-list-4bit-0 = 0
; [140] s-list-4bit-1 = 0
; [148] c-out-4bit = 1
; [148] s-list-4bit-2 = 0
; [156] s-list-4bit-3 = 0
; 計算時間は48 = 156 - 108
