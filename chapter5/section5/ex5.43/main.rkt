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
      '(env val proc argl continue)
      compile-operations
      insts
    )
  )

  (set-register-contents! machine 'env (setup-environment))
  (start machine)
)

; 以下のコードは問題なく実行できる
(define 
  test1
  '
  ((lambda (a) 
     (define (a) 2)
     (define b (+ (a) 1))
     b
   ) 
    (lambda () 3)
  )
)
(define 
  insts1
  (statements 
    (compile 
      test1
      'val
      'next
      (extend-compile-time-environment 
        (make-compile-time-frame the-empty-environment)
        '()
      )
    )
  )
)
(make-and-run-machine insts1)
; 'done

; 以下のコードはdefineに関する曖昧性がある。
; (+ (a) 1)で参照される(a)は、defineの定義が真に同時であれば2であるが、手続き的であれば3になる。
; 今回の演習の実装により、以下のような曖昧なコードはエラーにできる。
; 詳しくは演習問題4.19のAlyssaの主張を参照。
(define 
  test2
  '
  ((lambda (a) 
     (define b (+ a 1))
     (define (a) 2)
     b
   ) 
    (lambda () 3)
  )
)
(define 
  insts2
  (statements 
    (compile 
      test2
      'val
      'next
      (extend-compile-time-environment 
        (make-compile-time-frame the-empty-environment)
        '()
      )
    )
  )
)
(make-and-run-machine insts2)
; Lookup unassigned value
