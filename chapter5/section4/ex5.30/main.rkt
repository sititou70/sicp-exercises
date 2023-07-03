#lang racket
(provide (all-defined-out))

(require sicp)
(require "machine/make-machine.rkt")
(require "ec-eval/repl.rkt")
(require "ec-eval/eval-apply.rkt")

; main
(define inputs '())
(define 
  (read-inputs)
  (if (null? inputs) 
    (begin (displayln "no more input") (exit))
  )
  (let 
    ( ;
     (input (car inputs))
    )

    (set! inputs (cdr inputs))
    input
  )
)
(define 
  eceval
  (make-machine 
    '(exp env val proc argl continue unev)
    (append 
      (list 
        (list 'read read-inputs)
        (list 'prompt-for-input (lambda (_) '()))
      )
      repl-operations
      eval-operations
      apply-operations
    )
    (append repl-insts eval-insts apply-insts)
  )
)

(set! 
  inputs
  '
  ( ;
   ; 以下は様々なエラーを発生させるが、REPLは終了しない
   #t
   ; {ec-eval-error unknown expression type}
   (123 1 2 3)
   ; {ec-eval-error unknown procedure type}
   x
   ; {ec-eval-error Unbound variable x}
   (set! x 123)
   ; {ec-eval-error Unbound variable: SET! x}
   (cond 
     (else 1)
     (true 2)
   )
   ; {ec-eval-error ELSE clause isn't last: COND->IF {{else 1} {true 2}}}
   ((lambda (x) x) 1 2)
   ; {ec-eval-error Too many arguments supplied {x} {1 2}}
   (/ 1 0)
   ; {ec-eval-error apply-primitive-procedure #(struct:exn:fail:contract:divide-by-zero /: division by zero #<continuation-mark-set>)}
   (car 'abc)
   ; {ec-eval-error apply-primitive-procedure #(struct:exn:fail:contract mcar: contract violation
   ;   expected: mpair?
   ;   given: 'abc #<continuation-mark-set>)}

   ; 引き続き正常なコードを実行できる
   (define 
     (append x y)
     (if (null? x) 
       y
       (cons 
         (car x)
         (append (cdr x) y)
       )
     )
   )
   (append '(a b c) '(d e f))
   ; {a b c d e f}
  )
)

; a. 評価プロセスで発生するエラーは2種類ある。
; 1つは、(error ...)によって記述されるものである。
; これらのエラーはREPLに入力されたユーザープログラムが原因として発生するものである。
; したがって、REPLで補足し、REPL自体を終了させないようにするのが望ましい。
; もう1つの種類は上記以外のエラーであり、これは評価器自体のバグによるエラーである。
; これはユーザー環境自体のバグであるため、REPLを継続するのではなくクラッシュさせるのが望ましい

; したがって、修正箇所は(error ...)の部分だけでよい。
; (error ...)を独自のエラー型である(make-error ...)に置き換え、評価器自身でハンドリングするようにした。
; エラーはec-eval-errorというタグで識別するこことした。ec-eval-errorはユーザープログラムに現れないとか仮定している。

; b. 基本手続きに関しては、Racketのエラーハンドリング機能を使用した。
; キャッチしたエラーを独自のエラー型に置き換え、REPLでハンドリングするようにした。

; 注意：問題文には「大変な作業」や「一大プロジェクト」とあるが、そこまでの作業量があったとは思えない。したがって、題意をうまく理解できていない可能性がある。
; または、他の処理系では基層のエラーハンドリングに関する事情が違うため、より手間がかかるのかもしれない。
; 本実装では、基本手続きのエラー処理は基層のものをそのまま流用している。

(start eceval)
