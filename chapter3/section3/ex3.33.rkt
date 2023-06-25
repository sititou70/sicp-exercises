#lang racket/base
(require sicp)

; connector
(define 
  (make-connector)

  (define 
    (for-each-except exception procedure list)
    (define 
      (loop items)
      (cond 
        ((null? items) 'done)
        ((eq? (car items) exception) (loop (cdr items)))
        (else
         (procedure (car items))
         (loop (cdr items))
        )
      )
    )
    (loop list)
  )

  (let 
    ((value false) (informant false) (constraints '()))
    (define 
      (set-my-value newval setter)
      (cond 
        ((not (has-value? me))
         (set! value newval)
         (set! informant setter)
         (for-each-except 
           setter
           inform-about-value
           constraints
         )
        )
        ((not (= value newval))
         (error " Contradiction " (list value newval))
        )
        (else 'ignored)
      )
    )
    (define 
      (forget-my-value retractor)
      (if (eq? retractor informant) 
        (begin 
          (set! informant false)
          (for-each-except 
            retractor
            inform-about-no-value
            constraints
          )
        )
        'ignored
      )
    )
    (define 
      (connect new-constraint)
      (if (not (memq new-constraint constraints)) 
        (set! 
          constraints
          (cons new-constraint constraints)
        )
      )
      (if (has-value? me) 
        (inform-about-value new-constraint)
      )
      'done
    )
    (define 
      (me request)
      (cond 
        ((eq? request 'has-value?)
         (if informant true false)
        )
        ((eq? request 'value) value)
        ((eq? request 'set-value!) set-my-value)
        ((eq? request 'forget) forget-my-value)
        ((eq? request 'connect) connect)
        (else
         (error 
           " Unknown operation : CONNECTOR "
           request
         )
        )
      )
    )
    me
  )
)

(define 
  (inform-about-value constraint)
  (constraint 'I-have-a-value)
)
(define 
  (inform-about-no-value constraint)
  (constraint 'I-lost-my-value)
)

(define (has-value? connector) (connector 'has-value?))
(define (get-value connector) (connector 'value))
(define 
  (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant)
)
(define (forget-value! connector retractor) ((connector 'forget) retractor))
(define (connect connector new-constraint) ((connector 'connect) new-constraint))

; utils
(define 
  (probe name connector)
  (define 
    (print-probe value)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value)
    (newline)
  )
  (define 
    (process-new-value)
    (print-probe (get-value connector))
  )
  (define (process-forget-value) (print-probe "?"))
  (define 
    (me request)
    (cond 
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else (error " Unknown request : PROBE " request))
    )
  )
  (connect connector me)
  me
)

; basic object
(define 
  (averager a b c)
  (define 
    (process-new-value)
    (cond 
      ((and (has-value? a) (has-value? b))
       (set-value! 
         c
         (/ (+ (get-value a) (get-value b)) 2)
         me
       )
      )
      ((and (has-value? a) (has-value? c))
       (set-value! 
         b
         (- (* (get-value c) 2) (get-value a))
         me
       )
      )
      ((and (has-value? b) (has-value? c))
       (set-value! 
         a
         (- (* (get-value c) 2) (get-value b))
         me
       )
      )
    )
  )

  (define 
    (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (forget-value! c me)
    (process-new-value)
  )

  (define 
    (me request)
    (cond 
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else (error " Unknown request : ADDER " request))
    )
  )

  (connect a me)
  (connect b me)
  (connect c me)
  me
)

; main
(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(averager a b c)

(probe "a" a)
(probe "b" b)
(probe "c" c)

(set-value! a 6 'user)
; Probe: a = 6

(set-value! b 8 'user)
; Probe: b = 8
; Probe: c = 7

(forget-value! a 'user)
; Probe: a = ?
; Probe: c = ?

(set-value! c 5 'user)
; Probe: c = 5
; Probe: a = 2
