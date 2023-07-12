#lang racket
(provide (all-defined-out))

(require sicp)
(require "machine/make-machine.rkt")
(require "compiler/environment.rkt")
(require "compiler/compile.rkt")
(require "compiler/instruction-sequence.rkt")
(require "compiler/global-environment.rkt")

; main
(define 
  (make-and-run-machine insts)

  (define 
    machine
    (make-machine 
      '(env val proc argl continue arg1 arg2)
      compile-operations
      insts
    )
  )

  (set-register-contents! machine 'env (setup-environment))
  (start machine)
)

(define 
  (compile-program program)
  (statements 
    (compile 
      program
      'val
      'next
      (extend-compile-time-environment 
        (make-compile-time-frame the-empty-environment)
        '()
      )
    )
  )
)

; 以下のコードはオープンコードされる
(define 
  test1
  '
  (lambda (x) (+ 1 2))
)
(map displayln (compile-program test1))
; {assign val {op make-compiled-procedure} {label entry1} {reg env}}
; {goto {label after-lambda2}}
;
; entry1
; {assign env {op compiled-procedure-env} {reg proc}}
; {assign env {op extend-environment} {const {x}} {reg argl} {reg env}}
; {assign arg1 {const 1}}
; {assign arg2 {const 2}}
; {assign val {op +} {reg arg1} {reg arg2}}
; {goto {reg continue}}
;
; after-lambda2

; 以下のコードはオープンコードされない
(define 
  test2
  '
  (lambda (+) (+ 1 2))
)
(map displayln (compile-program test2))
; {assign val {op make-compiled-procedure} {label entry3} {reg env}}
; {goto {label after-lambda4}}
;
; entry3
; {assign env {op compiled-procedure-env} {reg proc}}
; {assign env {op extend-environment} {const {+}} {reg argl} {reg env}}
; {assign proc {op lexical-address-lookup} {const {0 . 0}} {reg env}}
; {assign val {const 2}}
; {assign argl {op list} {reg val}}
; {assign val {const 1}}
; {assign argl {op cons} {reg val} {reg argl}}
; {test {op primitive-procedure?} {reg proc}}
; {branch {label primitive-branch5}}
;
; compiled-branch6
; {assign val {op compiled-procedure-entry} {reg proc}}
; {goto {reg val}}
;
; primitive-branch5
; {assign val {op apply-primitive-procedure} {reg proc} {reg argl}}
; {goto {reg continue}}
;
; after-call7
; after-lambda4

; 本文のコードを試してみる
(define 
  test3
  '
  (begin 
    (define 
      (+matrix mat1 mat2)
      (list 
        (list 
          (+ (caar mat1) (caar mat2))
          (+ (cadar mat1) (cadar mat2))
        )
        (list 
          (+ (caadr mat1) (caadr mat2))
          (+ (cadadr mat1) (cadadr mat2))
        )
      )
    )

    (define 
      (*matrix scalar mat)
      (list 
        (list 
          (* scalar (caar mat))
          (* scalar (cadar mat))
        )
        (list 
          (* scalar (caadr mat))
          (* scalar (cadadr mat))
        )
      )
    )

    (displayln 
      ((lambda (+ * a b x y) 
         (+ (* a x) (* b y))
       ) 
        +matrix
        *matrix
        2
        3
        '((1 2) (3 4))
        '((5 6) (7 8))
      )
    )
    ; {{17 22} {27 32}}
  )
)
(make-and-run-machine (compile-program test3))
