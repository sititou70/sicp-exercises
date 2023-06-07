#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "m-eval/eval-apply.scm")
(require "m-eval/global-environment.scm")

(define a-val actual-value)

; main
(a-val 
  '(define
    (f a (b lazy) c (d lazy-memo))

    (displayln "f called")
    (if a true) ; 各引数を強制するために基本構文であるifの条件部分を使っている
    (if a true)
    (if b true)
    (if b true)
    (if c true)
    (if c true)
    (if d true)
    (if d true)
    'done
   )
  the-global-environment
)
; 'ok

(a-val 
  '(f
    (displayln "a")
    (displayln "b")
    (displayln "c")
    (displayln "d")
   )
  the-global-environment
)
; a
; c
; f called
; b
; b
; d
; 'done

; a、cは通常の引数であるため、適用順序によってfが呼び出される前に評価されている
; bは遅延化されているがメモ化されていないオブジェクトであるので、評価のたびにdisplaylnが実行されている
; dは遅延化およびメモ化されたオブジェクトであるため、displaylnは1度だけ実行されている
