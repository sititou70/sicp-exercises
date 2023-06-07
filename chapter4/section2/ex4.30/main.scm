#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require (only-in "m-eval/eval-apply.scm" actual-value))
(require (only-in "m-eval/eval-apply-cy.scm" [actual-value actual-value-cy]))
(require "m-eval/global-environment.scm")

(define a-val actual-value)
(define a-val-cy actual-value-cy)

; main
; Cyは、eval-sequence を修正し、eval でなく actual-value を使用したほうがよいと提案した。
; a. Benはfor-eachについて、もとの評価器でも正しく扱えると主張する。
(a-val 
  '(define
    (for-each proc items)
    (if (null? items) 
      'done
      (begin 
        (proc (car items))
        (for-each proc (cdr items))
      )
    )
   )
  the-global-environment
)
; 'ok
(a-val 
  '(for-each
    (lambda (x) (newline) (display x))
    (list 57 321 88)
   )
  the-global-environment
)
; 
; 57
; 321
; 88'done

; ここまでの処理におけるeval-sequenceの適用をトレースする。
; まず、for-eachのbodyに適用されるが、これはifだけの列であり、長さが1であるからCyの変更との違いはない
; 次に、beginのactionsに対して適用される。列の1つめはprocの適用である。
; (car items)が遅延オブジェクトとなり、((newline) (display x))という列に対してeval-sequenceが適用される。
; しかし、newlineとdisplayはどちらも基本オブジェクトであるので、結局は演算子も被演算子もすべて強制されることとなり、evalとactial-valueの挙動に違いはない。
; 列の最後は、for-eachの適用である。これは列の最後の要素であるので、Cyの実装と違いがない。
; したがって、上記の処理はもとの評価器でもうまくいく

; b. Cy は、for-each については Ben が正しいと認めたが、次の二つの手続きを定義し、それぞれの評価器での挙動の違いを主張した。
(a-val 
  '(define
    (p1 x)
    (set! x (cons x '(2)))
    x
   )
  the-global-environment
)
(a-val '(p1 1) the-global-environment)
; (mcons 1 (mcons 2 '()))
(a-val-cy '(p1 1) the-global-environment)
; (mcons 1 (mcons 2 '()))

; p1のbodyの列の1つ目の要素はset!である。これは適用ではなく構文手続きであるため、evalとactual-valueの挙動に違いがない
; 2つ目の要素はxであり、これは最後の要素であるため、2つの評価器の挙動に違いはない。

(a-val 
  '(define
    (p2 x)
    (define 
      (p e)
      e
      x
    )
    (p (set! x (cons x ' (2))))
   )
  the-global-environment
)
(a-val '(p2 1) the-global-environment)
; 1
(a-val-cy '(p2 1) the-global-environment)
; (mcons 1 (mcons 2 '()))

; 挙動の違いはp手続きのbodyの列で発生している。列の1つめの要素であるeは、遅延化された(set! x (cons x ' (2)))である。
; evalとactual-valueでは、これをforceするかどうかに違いがあるため、他方では副作用が実行されなかった。

; c. Cyは、自身が提案する変更はaの例の振る舞いにも影響を与えないと言った。説明せよ

; actual-valueは、遅延化されていないオブジェクトに適用してもその値を即座に返すだけでありエラーにはならない。
; したがって、(proc ... )の評価結果が遅延化されていない値だとしても、そうでないとしても、エラーにはならない。
; 「ふるまいに影響を与えない」という表現は少し行き過ぎかもしれないが、Cyの考える「望ましい挙動」は達成できるといえる。

; d. あなたの考えでは、遅延評価器で列を扱うのはどのようにするのがよいだろうか。

; オリジナルの挙動を採用した上で、副作用を持つ遅延化オブジェクトが発生しないようにプログラムを作成するのが望ましいと考える。
; Cyの提案は、遅延評価と副作用の混乱しがちな問題を一部解消しているものの完璧ではない。
; また、評価機の挙動として少し一貫性がない。「プログラムの書き方によって一部は遅延評価で無くなる」というのは逆に混乱を生むかもしれない。
