#lang racket
(provide (all-defined-out))

(require sicp)
(require "compiler/compile.rkt")
(require "compiler/instruction-sequence.rkt")

; main
(define 
  program
  '
  (begin 
    (define 
      (f a b c)
      (list a b c)
    )
    (f 1 2 3)
  )
)
; 通常のコンパイル：saveおよびrestore命令は含まれない
; 引数が自己評価式であったり、手続き適用が末尾呼び出しであったりするため、レジスタを保存/復元する必要がないからである。
(map displayln (statements (compile program 'val 'next)))
; {assign val {op make-compiled-procedure} {label entry1} {reg env}}
; {goto {label after-lambda2}}

; entry1
; {assign env {op compiled-procedure-env} {reg proc}}
; {assign env {op extend-environment} {const {a b c}} {reg argl} {reg env}}
; {assign proc {op lookup-variable-value} {const list} {reg env}}
; {assign val {op lookup-variable-value} {const c} {reg env}}
; {assign argl {op list} {reg val}}
; {assign val {op lookup-variable-value} {const b} {reg env}}
; {assign argl {op cons} {reg val} {reg argl}}
; {assign val {op lookup-variable-value} {const a} {reg env}}
; {assign argl {op cons} {reg val} {reg argl}}
; {test {op primitive-procedure?} {reg proc}}
; {branch {label primitive-branch3}}

; compiled-branch4
; {assign val {op compiled-procedure-entry} {reg proc}}
; {goto {reg val}}

; primitive-branch3
; {assign val {op apply-primitive-procedure} {reg proc} {reg argl}}
; {goto {reg continue}}
; after-call5
; after-lambda2
; {perform {op define-variable!} {const f} {reg val} {reg env}}
; {assign val {const ok}}
; {assign proc {op lookup-variable-value} {const f} {reg env}}
; {assign val {const 3}}
; {assign argl {op list} {reg val}}
; {assign val {const 2}}
; {assign argl {op cons} {reg val} {reg argl}}
; {assign val {const 1}}
; {assign argl {op cons} {reg val} {reg argl}}
; {test {op primitive-procedure?} {reg proc}}
; {branch {label primitive-branch6}}

; compiled-branch7
; {assign continue {label after-call8}}
; {assign val {op compiled-procedure-entry} {reg proc}}
; {goto {reg val}}

; primitive-branch6
; {assign val {op apply-primitive-procedure} {reg proc} {reg argl}}
; after-call8

(use-verbose-preserving)

; 冗長なpreservingによるコンパイル：余分なsave/restoreのペアが31個含まれる
(map displayln (statements (compile program 'val 'next)))
; {save continue}
; {save env}
; {save continue}
; {save env}
; {save continue}
; {assign val {op make-compiled-procedure} {label entry9} {reg env}}
; {restore continue}
; {goto {label after-lambda10}}
; entry9
; {assign env {op compiled-procedure-env} {reg proc}}
; {assign env {op extend-environment} {const {a b c}} {reg argl} {reg env}}
; {save continue}
; {save env}
; {save continue}
; {assign proc {op lookup-variable-value} {const list} {reg env}}
; {restore continue}
; {restore env}
; {restore continue}
; {save continue}
; {save proc}
; {save env}
; {save continue}
; {assign val {op lookup-variable-value} {const c} {reg env}}
; {restore continue}
; {assign argl {op list} {reg val}}
; {restore env}
; {save env}
; {save argl}
; {save continue}
; {assign val {op lookup-variable-value} {const b} {reg env}}
; {restore continue}
; {restore argl}
; {assign argl {op cons} {reg val} {reg argl}}
; {restore env}
; {save argl}
; {save continue}
; {assign val {op lookup-variable-value} {const a} {reg env}}
; {restore continue}
; {restore argl}
; {assign argl {op cons} {reg val} {reg argl}}
; {restore proc}
; {restore continue}
; {test {op primitive-procedure?} {reg proc}}
; {branch {label primitive-branch11}}
; compiled-branch12
; {assign val {op compiled-procedure-entry} {reg proc}}
; {goto {reg val}}
; primitive-branch11
; {save continue}
; {assign val {op apply-primitive-procedure} {reg proc} {reg argl}}
; {restore continue}
; {goto {reg continue}}
; after-call13
; after-lambda10
; {restore env}
; {perform {op define-variable!} {const f} {reg val} {reg env}}
; {assign val {const ok}}
; {restore continue}
; {restore env}
; {restore continue}
; {save continue}
; {save env}
; {save continue}
; {assign proc {op lookup-variable-value} {const f} {reg env}}
; {restore continue}
; {restore env}
; {restore continue}
; {save continue}
; {save proc}
; {save env}
; {save continue}
; {assign val {const 3}}
; {restore continue}
; {assign argl {op list} {reg val}}
; {restore env}
; {save env}
; {save argl}
; {save continue}
; {assign val {const 2}}
; {restore continue}
; {restore argl}
; {assign argl {op cons} {reg val} {reg argl}}
; {restore env}
; {save argl}
; {save continue}
; {assign val {const 1}}
; {restore continue}
; {restore argl}
; {assign argl {op cons} {reg val} {reg argl}}
; {restore proc}
; {restore continue}
; {test {op primitive-procedure?} {reg proc}}
; {branch {label primitive-branch14}}
; compiled-branch15
; {assign continue {label after-call16}}
; {assign val {op compiled-procedure-entry} {reg proc}}
; {goto {reg val}}
; primitive-branch14
; {save continue}
; {assign val {op apply-primitive-procedure} {reg proc} {reg argl}}
; {restore continue}
; after-call16
