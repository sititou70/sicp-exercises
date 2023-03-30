#lang racket/base

(define 
  (square x)
  (* x x)
)

; Louis Reasoner は練習問題 2.21の一つ目の square-list 手続きを書き直し、反復プロセスを展開するようにしようとしている。
(define 
  (square-list1 items)

  (define 
    (iter things answer)
    (if (null? things) 
      answer
      (iter 
        (cdr things)
        (cons (square (car things)) 
              answer
        )
      )
    )
  )

  (iter items null)
)

(square-list1 (list 1 2 3 4))
; '(16 9 4 1)
; 逆順になってしまう
; iterではリストを先頭から捜査しているが、(cons (square (car things)) answer)において、これまでの計算結果であるanswerをconsの後ろに置いているからである

; それから、Louis は cons の引数を逆順にしてバグを直そうとした。
(define 
  (square-list2 items)

  (define 
    (iter things answer)
    (if (null? things) 
      answer
      (iter 
        (cdr things)
        (cons answer 
              (square (car things))
        )
      )
    )
  )

  (iter items null)
)

(square-list2 (list 1 2 3 4))
; '((((() . 1) . 4) . 9) . 16)
; これもうまく行かない
; (cons answer (square (car things)))という書き方では、ペアの最初の要素に次のペアが来てしまっている。
; これは一般的なリストの構造になっていない。
