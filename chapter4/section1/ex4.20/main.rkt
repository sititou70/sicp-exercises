#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "m-eval/eval-apply.rkt")
(require "m-eval/global-environment.rkt")

; main
; 評価器はletをサポートしている
(eval 
  '(let
    ( ;
     (a 1)
    )
    a
   )
  the-global-environment
)
; 1

; letrec
(eval 
  '(letrec
    ( ;
     (a 1)
     (b 2)
    )
    (cons a b)
   )
  the-global-environment
)
; (mcons 1 2)

(eval 
  '(define
    (f x)
    (letrec 
      ( ;
       (even? 
         (lambda (n) 
           (if (= n 0) true (odd? (- n 1)))
         )
       )
       (odd? 
         (lambda (n) 
           (if (= n 0) false (even? (- n 1)))
         )
       )
      )
      (even? x)
    )
   )
  the-global-environment
)
; 'ok

(eval '(f 10) the-global-environment)
; #t
(eval '(f 11) the-global-environment)
; #f
(eval '(f 12) the-global-environment)
; #t
(eval '(f 13) the-global-environment)
; #f

; fをletrecで定義すると以下のように展開される
; (define 
;   (f x)
;   (let 
;     ( ;
;      (even? '*unassigned*)
;      (odd? '*unassigned*)
;     )
; 
;     (set! 
;       even?
;       (lambda (n) 
;         (if (= n 0) true (odd? (- n 1)))
;       )
;     )
;     (set! 
;       odd?
;       (lambda (n) 
;         (if (= n 0) false (even? (- n 1)))
;       )
;     )
;     (even? x)
;   )
; )

; これは更に、次のように展開される
; (define 
;   (f x)
;   ((lambda (even? odd?) 
;      (set! 
;        even?
;        (lambda (n) 
;          (if (= n 0) true (odd? (- n 1)))
;        )
;      )
;      (set! 
;        odd?
;        (lambda (n) 
;          (if (= n 0) false (even? (- n 1)))
;        )
;      )
;      (even? x)
;    ) 
;     '*unassigned*
;     '*unassigned*
;   )
; )

; ここで(f 5)を評価しようとすると以下のような環境図になる

; global env --------------------+
; | f:  primitive procedures ... |
; +------------------------------+
; 
;   f
;   |
;   v
; (.)(.)--> global env
;  |
;  v
; parameters: x
; body: (letrec ... )
; 
; E1 ((lambda (even? odd?) ... )) --+
; | even?:                          |--> global env
; | odd?:                           |
; +---------------------------------+
; 
; even?
;   |
;   v
; (.)(.)--> E1
;  |
;  v
; parameters: n
; body: (if (= n 0) true (odd? (- n 1)))
; 
; odd?
;   |
;   v
; (.)(.)--> E1
;  |
;  v
; parameters: n
; body: (if (= n 0) false (even? (- n 1)))

; (even? x)はE1で評価される。

; 一方、fを単なるletで定義すると次のようになる
; (define 
;   (f x)
;   (let 
;     ( ;
;      (even? 
;        (lambda (n) 
;          (if (= n 0) true (odd? (- n 1)))
;        )
;      )
;      (odd? 
;        (lambda (n) 
;          (if (= n 0) false (even? (- n 1)))
;        )
;      )
;     )
; 
;     (even? x)
;   )
; )

; これは更に、次のように展開される
; (define 
;   (f x)
;   ((lambda (even? odd?) 
; 
;      (even? x)
;    ) 
;     (lambda (n) 
;       (if (= n 0) true (odd? (- n 1)))
;     )
;     (lambda (n) 
;       (if (= n 0) false (even? (- n 1)))
;     )
;   )
; )

; 環境図は以下のようになる。

; global env --------------------+
; | f:  primitive procedures ... |
; +------------------------------+
; 
;   f
;   |
;   v
; (.)(.)--> global env
;  |
;  v
; parameters: x
; body: (letrec ... )
; 
; E1 ((lambda (even? odd?) ... )) --+
; | even?:                          |--> global env
; | odd?:                           |
; +---------------------------------+
; 
; even?
;   |
;   v
; (.)(.)--> global env
;  |
;  v
; parameters: n
; body: (if (= n 0) true (odd? (- n 1)))
; 
; odd?
;   |
;   v
; (.)(.)--> global env
;  |
;  v
; parameters: n
; body: (if (= n 0) false (even? (- n 1)))

; (even? x)はE1で評価される。
; ここで、even?の外側の環境はglobal envであるため、odd?は見つからない。
; これがLouisの考えの穴である。
