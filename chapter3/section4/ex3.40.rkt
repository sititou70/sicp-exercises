#lang racket/base
(require sicp)

(define 
  (make-commands)

  (define 
    (iter a-step b-step command)
    (if (and (= a-step 3) (= b-step 4)) 
      (list command)
      (append 
        (if (= a-step 3) 
          '()
          (iter (+ a-step 1) b-step (cons (cons 'a a-step) command))
        )
        (if (= b-step 4) 
          '()
          (iter a-step (+ b-step 1) (cons (cons 'b b-step) command))
        )
      )
    )
  )

  (map reverse (iter 0 0 '()))
)

(define 
  (evaluate commands)

  (define x 10)
  (define a0 '())
  (define a1 '())
  (define a2 '())
  (define b0 '())
  (define b1 '())
  (define b2 '())
  (define b3 '())

  (newline)
  (display commands)
  (for-each 
    (lambda (command) 
      (cond 
        ((and 
           (eq? (car command) 'a)
           (= (cdr command) 0)
         )
         (set! a0 x)
        )
        ((and 
           (eq? (car command) 'a)
           (= (cdr command) 1)
         )
         (set! a1 x)
        )
        ((and 
           (eq? (car command) 'a)
           (= (cdr command) 2)
         )
         (set! x (* a0 a1))
        )
        ((and 
           (eq? (car command) 'b)
           (= (cdr command) 0)
         )
         (set! b0 x)
        )
        ((and 
           (eq? (car command) 'b)
           (= (cdr command) 1)
         )
         (set! b1 x)
        )
        ((and 
           (eq? (car command) 'b)
           (= (cdr command) 2)
         )
         (set! b2 x)
        )
        ((and 
           (eq? (car command) 'b)
           (= (cdr command) 3)
         )
         (set! x (* b0 b1 b2))
        )
      )
    )
    commands
  )

  (display "：")
  (display x)
)

(define commands (make-commands))
(for-each evaluate commands)
