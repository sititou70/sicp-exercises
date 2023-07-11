#lang racket
(provide (all-defined-out))

(require sicp)
(require "machine/make-machine.rkt")
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
  (get-register-contents machine 'val)
)

(define 
  insts1
  (statements 
    (compile 
      '(+ 1 2 3)
      'val
      'next
    )
  )
)
(map displayln insts1)
; {assign arg1 {const 1}}
; {assign arg2 {const 2}}
; {assign arg1 {op +} {reg arg1} {reg arg2}}
; {assign arg2 {const 3}}
; {assign val {op +} {reg arg1} {reg arg2}}
(make-and-run-machine insts1)
; 6

; テスト2
(define 
  insts2
  (statements 
    (compile 
      '(+ (+ 1 2 3) (+ 4 5 6) (+ 7 8 9))
      'val
      'next
    )
  )
)
(map displayln insts2)
; {assign arg1 {const 1}}
; {assign arg2 {const 2}}
; {assign arg1 {op +} {reg arg1} {reg arg2}}
; {assign arg2 {const 3}}
; {assign arg1 {op +} {reg arg1} {reg arg2}}
; {save arg1}
; {assign arg1 {const 4}}
; {assign arg2 {const 5}}
; {assign arg1 {op +} {reg arg1} {reg arg2}}
; {assign arg2 {const 6}}
; {assign arg2 {op +} {reg arg1} {reg arg2}}
; {restore arg1}
; {assign arg1 {op +} {reg arg1} {reg arg2}}
; {save arg1}
; {assign arg1 {const 7}}
; {assign arg2 {const 8}}
; {assign arg1 {op +} {reg arg1} {reg arg2}}
; {assign arg2 {const 9}}
; {assign arg2 {op +} {reg arg1} {reg arg2}}
; {restore arg1}
; {assign val {op +} {reg arg1} {reg arg2}}
(make-and-run-machine insts2)
; 45

; factorialをコンパイルしてみる
(define 
  insts3
  (statements 
    (compile 
      '
      (define 
        (factorial n)
        (if (= n 1) 1 (* (factorial (- n 1)) n))
      )
      'val
      'next
    )
  )
)
(map displayln insts3)
; {assign val {op make-compiled-procedure} {label entry1} {reg env}}
; {goto {label after-lambda2}}
; 
; entry1
; {assign env {op compiled-procedure-env} {reg proc}}
; {assign env {op extend-environment} {const {n}} {reg argl} {reg env}}
; {assign arg1 {op lookup-variable-value} {const n} {reg env}}
; {assign arg2 {const 1}}
; {assign val {op =} {reg arg1} {reg arg2}}
; {test {op false?} {reg val}}
; {branch {label false-branch4}}
; 
; true-branch3
; {assign val {const 1}}
; {goto {reg continue}}
; 
; false-branch4
; {save continue}
; {save env}
; {assign proc {op lookup-variable-value} {const factorial} {reg env}}
; {assign arg1 {op lookup-variable-value} {const n} {reg env}}
; {assign arg2 {const 1}}
; {assign val {op -} {reg arg1} {reg arg2}}
; {assign argl {op list} {reg val}}
; {test {op primitive-procedure?} {reg proc}}
; {branch {label primitive-branch6}}
; 
; compiled-branch7
; {assign continue {label proc-return9}}
; {assign val {op compiled-procedure-entry} {reg proc}}
; {goto {reg val}}
; 
; proc-return9
; {assign arg1 {reg val}}
; {goto {label after-call8}}
; 
; primitive-branch6
; {assign arg1 {op apply-primitive-procedure} {reg proc} {reg argl}}
; 
; after-call8
; {restore env}
; {assign arg2 {op lookup-variable-value} {const n} {reg env}}
; {assign val {op *} {reg arg1} {reg arg2}}
; {restore continue}
; {goto {reg continue}}
; 
; after-if5
; after-lambda2
; {perform {op define-variable!} {const factorial} {reg val} {reg env}}
; {assign val {const ok}}

; オープンコードなしでfactorialをコンパイルした結果は以下の通り
; {assign val {op make-compiled-procedure} {label entry183} {reg env}}
; {goto {label after-lambda184}}
; 
; entry183
; {assign env {op compiled-procedure-env} {reg proc}}
; {assign env {op extend-environment} {const {n}} {reg argl} {reg env}}
; {save continue}
; {save env}
; {assign proc {op lookup-variable-value} {const =} {reg env}}
; {assign val {const 1}}
; {assign argl {op list} {reg val}}
; {assign val {op lookup-variable-value} {const n} {reg env}}
; {assign argl {op cons} {reg val} {reg argl}}
; {test {op primitive-procedure?} {reg proc}}
; {branch {label primitive-branch188}}
; 
; compiled-branch189
; {assign continue {label after-call190}}
; {assign val {op compiled-procedure-entry} {reg proc}}
; {goto {reg val}}
; 
; primitive-branch188
; {assign val {op apply-primitive-procedure} {reg proc} {reg argl}}
; 
; after-call190
; {restore env}
; {restore continue}
; {test {op false?} {reg val}}
; {branch {label false-branch186}}
; 
; true-branch185
; {assign val {const 1}}
; {goto {reg continue}}
; 
; false-branch186
; {assign proc {op lookup-variable-value} {const *} {reg env}}
; {save continue}
; {save proc}
; {assign val {op lookup-variable-value} {const n} {reg env}}
; {assign argl {op list} {reg val}}
; {save argl}
; {assign proc {op lookup-variable-value} {const factorial} {reg env}}
; {save proc}
; {assign proc {op lookup-variable-value} {const -} {reg env}}
; {assign val {const 1}}
; {assign argl {op list} {reg val}}
; {assign val {op lookup-variable-value} {const n} {reg env}}
; {assign argl {op cons} {reg val} {reg argl}}
; {test {op primitive-procedure?} {reg proc}}
; {branch {label primitive-branch191}}
; 
; compiled-branch192
; {assign continue {label after-call193}}
; {assign val {op compiled-procedure-entry} {reg proc}}
; {goto {reg val}}
; 
; primitive-branch191
; {assign val {op apply-primitive-procedure} {reg proc} {reg argl}}
; 
; after-call193
; {assign argl {op list} {reg val}}
; {restore proc}
; {test {op primitive-procedure?} {reg proc}}
; {branch {label primitive-branch194}}
; 
; compiled-branch195
; {assign continue {label after-call196}}
; {assign val {op compiled-procedure-entry} {reg proc}}
; {goto {reg val}}
; 
; primitive-branch194
; {assign val {op apply-primitive-procedure} {reg proc} {reg argl}}
; 
; after-call196
; {restore argl}
; {assign argl {op cons} {reg val} {reg argl}}
; {restore proc}
; {restore continue}
; {test {op primitive-procedure?} {reg proc}}
; {branch {label primitive-branch197}}
; 
; compiled-branch198
; {assign val {op compiled-procedure-entry} {reg proc}}
; {goto {reg val}}
; 
; primitive-branch197
; {assign val {op apply-primitive-procedure} {reg proc} {reg argl}}
; {goto {reg continue}}
; 
; after-call199
; after-if187
; after-lambda184
; {perform {op define-variable!} {const factorial} {reg val} {reg env}}
; {assign val {const ok}}

(make-and-run-machine 
  (statements 
    (compile 
      '
      (begin 
        (define 
          (factorial n)
          (if (= n 1) 
            1
            (* (factorial (- n 1)) n)
          )
        )
        (factorial 10)
      )
      'val
      'next
    )
  )
)
; 3628800

; オープンコードで命令数を削減できているとわかる。factrial手続きの命令数を比較すると以下のようになる。
; オープンコードあり：33
; オープンコードなし：62
