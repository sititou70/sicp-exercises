#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "eval-apply.scm")
(require "global-environment.scm")

; main
(eval 
  '(define
    (t label)
    (display "t ")
    (displayln label)
    label
   )
  the-global-environment
)
; 'ok
(eval 
  '(define
    (f label)
    (display "f ")
    (displayln label)
    false
   )
  the-global-environment
)
; 'ok

; andとorを特殊形式で実装した場合
(displayln "case 1")
(eval '(and) the-global-environment)
; #t

(displayln "case 2")
(eval '(and (t 1) (t 2) (t 3)) the-global-environment)
; t 1
; t 2
; t 3
; 3

(displayln "case 3")
(eval '(and (t 1) (f 2) (f 3)) the-global-environment)
; t 1
; f 2
; #f

(displayln "case 4")
(eval '(or) the-global-environment)
; #f

(displayln "case 5")
(eval '(or (f 1) (f 2) (f 3)) the-global-environment)
; f 1
; f 2
; f 3
; #f

(displayln "case 6")
(eval '(or (f 1) (t 2) (t 3)) the-global-environment)
; f 1
; t 2
; 2

; andとorを派生式で実装した場合
(use-eval2)
(displayln "case 1")
(eval '(and) the-global-environment)
; #t

(displayln "case 2")
(eval '(and (t 1) (t 2) (t 3)) the-global-environment)
; t 1
; t 2
; t 3
; 3

(displayln "case 3")
(eval '(and (t 1) (f 2) (f 3)) the-global-environment)
; t 1
; f 2
; #f

(displayln "case 4")
(eval '(or) the-global-environment)
; #f

(displayln "case 5")
(eval '(or (f 1) (f 2) (f 3)) the-global-environment)
; f 1
; f 2
; f 3
; #f

(displayln "case 6")
(eval '(or (f 1) (t 2) (t 3)) the-global-environment)
; f 1
; t 2
; 2
