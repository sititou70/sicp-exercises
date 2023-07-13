#lang racket
(provide (all-defined-out))

(require sicp)
(require "machine/make-machine.rkt")
(require "compiler/compile.rkt")
(require "compiler/instruction-sequence.rkt")
(require "compiler/global-environment.rkt")

; main
(define 
  (make-and-run-machine program)

  (define 
    machine
    (make-machine 
      '(env val proc argl continue)
      compile-operations
      (statements (compile program 'val 'next))
    )
  )

  (set-register-contents! machine 'env (setup-environment))
  (start machine)
  (displayln (get-register-contents machine 'val))
  (machine 'print-stack-statistics)
)

(define 
  (compile-and-start-factorial n)
  (make-and-run-machine 
    `
    (begin 
      (define 
        (factorial n)
        (if (= n 1) 
          1
          (* (factorial (- n 1)) n)
        )
      )
      (factorial ,n)
    )
  )
)

(compile-and-start-factorial 1)
(compile-and-start-factorial 2)
(compile-and-start-factorial 3)
(compile-and-start-factorial 4)
(compile-and-start-factorial 5)
(compile-and-start-factorial 6)
(compile-and-start-factorial 7)
(compile-and-start-factorial 8)
(compile-and-start-factorial 9)
; 1
; {total-pushes = 2 maximum-depth = 2}
; 2
; {total-pushes = 8 maximum-depth = 5}
; 6
; {total-pushes = 14 maximum-depth = 8}
; 24
; {total-pushes = 20 maximum-depth = 11}
; 120
; {total-pushes = 26 maximum-depth = 14}
; 720
; {total-pushes = 32 maximum-depth = 17}
; 5040
; {total-pushes = 38 maximum-depth = 20}
; 40320
; {total-pushes = 44 maximum-depth = 23}
; 362880
; {total-pushes = 50 maximum-depth = 26}

; 結果のまとめはnote.mdに書いた
