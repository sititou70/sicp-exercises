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

; 先頭にitemを挿入するように変更する
(define 
  (insert-queue! queue item)
  (let 
    ((new-pair (cons item (front-ptr queue))))
    (cond 
      ((empty-queue? queue)
       (set-front-ptr! queue new-pair)
       (set-rear-ptr! queue new-pair)
       queue
      )
      (else
       (set-front-ptr! queue new-pair)
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
; キューの取り出し順がおかしいと、誤ったシミュレーション結果が得られる場合がある。
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define a1 (make-wire))
(define a2 (make-wire))
(define output (make-wire))
(and-gate a1 a2 output)

; 初期状態が安定するまで待つ
(set-signal! a1 0)
(set-signal! a2 1)
(propagate)
(probe 'output output)
; [3] output = 0

(set-signal! a1 1)
; ここで、and-gate内のand-action-procedureでは、回路からa1 = 1、a2 = 1という値が得られる。
; したがって、output = 1と計算され、and-gate-delay後にoutputを1に変更するようスケジュールされる。
(set-signal! a2 0)
; ここで、and-gate内のand-action-procedureでは、回路からa1 = 1、a2 = 0という値が得られる。
; したがって、output = 0と計算され、and-gate-delay後にoutputを1に変更するようスケジュールされる。
(propagate)
; キューの処理が正常な場合、上記のスケジュールは順番通りに実行される。つまり、outputが1になり、次に0になるため、シミュレーション結果として正しい。
; しかし、キューの処理が正常でない場合、上記のスケジュールは逆に実行される。つまり、outputが0になり、次に1になるため、シミュレーション結果として正しくない。
; [6] output = 1
