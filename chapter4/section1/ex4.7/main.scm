#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "m-eval/eval-apply.scm")
(require "m-eval/global-environment.scm")

; main
; 練習問題4.6で実装したletを使った派生式としてlet*を定義すれば十分と思われる。

; 通常のlet
(eval 
  '(let
    ( ;
     (x 1)
     (y (+ 1 1))
    )

    (displayln "hello let")
    (+ (* y y) x)
   )
  the-global-environment
)
; hello let
; 5

; let*
(eval 
  '(let*
    ( ;
     (x 3)
     (y (+ x 2))
     (z (+ x y 5))
    )

    (displayln "hello let*")
    (* x z)
   )
  the-global-environment
)
; hello let*
; 39

; letとlet*のネスト
(eval 
  '(let
    ( ;
     (a 1)
    )

    (let 
      ( ;
       (a 1)
      )

      a
    )
   )
  the-global-environment
)
; 1

(eval 
  '(let
    ( ;
     (a 1)
    )

    (let* 
      ( ;
       (x 3)
       (y (+ x 2))
       (z (+ x y 5))
      )

      (* x z)
    )
   )
  the-global-environment
)
; 39

(eval 
  '(let*
    ( ;
     (x 3)
     (y (+ x 2))
     (z (+ x y 5))
    )

    (let 
      ( ;
       (a 1)
      )

      (* x z)
    )
   )
  the-global-environment
)
; 39

(eval 
  '(let*
    ( ;
     (x 3)
     (y (+ x 2))
     (z (+ x y 5))
    )

    (let* 
      ( ;
       (x 3)
       (y (+ x 2))
       (z (+ x y 5))
      )

      (* x z)
    )
   )
  the-global-environment
)
; 39
