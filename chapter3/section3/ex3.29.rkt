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

; a || b = not(not(a || b)) = not(not a && not b)
(define 
  (or-gate a1 a2 output)
  (let 
    ( ;
     (a1not (make-wire))
     (a2not (make-wire))
     (andres (make-wire))
    )

    (inverter a1 a1not)
    (inverter a2 a2not)
    (and-gate a1not a2not andres)
    (inverter andres output)
    'ok
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
; (define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define result (make-wire))
(or-gate input-1 input-2 result)

; 初期状態が安定するまで待つ
(propagate) 
(probe 'result result)
; [7] result = 0

(set-signal! input-1 1)
(propagate)
; [14] result = 1
; or-gate全体の遅延時間は7 = inverter-delay + and-gate-delay + inverter-delay

(set-signal! input-2 1)
(propagate)
; 1つめのnotと次のandの入力は変化するが、最後のnotの入力は変化しない。したがって、5 = inverter-delay + and-gate-delayが経過

(set-signal! input-1 0)
(propagate)
; 1つめのnotと次のandの入力は変化するが、最後のnotの入力は変化しない。したがって、5 = inverter-delay + and-gate-delayが経過

(set-signal! input-2 0)
(propagate)
; or-gate全体が変化するので7が経過
; [31] result = 0
