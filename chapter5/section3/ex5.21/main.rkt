#lang racket
(provide (all-defined-out))

(require sicp)
(require "machine/make-machine.rkt")

; main
(define 
  machine-a
  (make-machine 
    '(tree count tmp continue)
    (list 
      (list '+ +)
      (list 'not not)
      (list 'null? null?)
      (list 'pair? pair?)
      (list 'car car)
      (list 'cdr cdr)
    )
    '( ;
      ; 最終的にgotoするアドレスを指定
      (assign continue (label done))

      count-leaves
      ; treeがnullならcountは0
      (test (op null?) (reg tree))
      (branch (label immediate-zero))

      ; treeがペアでないならcountは1
      (assign tmp (op pair?) (reg tree))
      (test (op not) (reg tmp))
      (branch (label immediate-one))

      ; treeがペアなのでcar + cdrのcountを計算する
      ;; carについてのcount-leavesの再帰呼出し
      (save tree)
      (assign tree (op car) (reg tree))
      (save continue)
      (assign continue (label count-leaves-for-car-done))
      (goto (label count-leaves))

      count-leaves-for-car-done
      (restore continue)
      (restore tree)
      (save count)

      ;; cdrについてのcount-leavesの再帰呼出し
      (save tree)
      (assign tree (op cdr) (reg tree))
      (save continue)
      (assign continue (label count-leaves-for-cdr-done))
      (goto (label count-leaves))

      count-leaves-for-cdr-done
      (restore continue)
      (restore tree)
      (assign tmp (reg count))
      (restore count)
      (assign count (op +) (reg tmp) (reg count))
      (goto (reg continue))

      immediate-zero
      (assign count (const 0))
      (goto (reg continue))

      immediate-one
      (assign count (const 1))
      (goto (reg continue))

      done
     )
  )
)

(set-register-contents! 
  machine-a
  'tree
  '(1
    2
    3
    (4 5)
    ((6 7 8 9) 10)
    (11 (12 (13 14 15) 16 17) 18 (19 20) 21 22 23)
    24
    (25 (26 27 28 29) (30) ((31 32 33) 34 (35 36) 37 ((38 39) (40) (41 42))))
   )
)
(start machine-a)
(get-register-contents machine-a 'count)
; 42

(define 
  machine-b
  (make-machine 
    '(tree count tmp continue)
    (list 
      (list '+ +)
      (list 'not not)
      (list 'null? null?)
      (list 'pair? pair?)
      (list 'car car)
      (list 'cdr cdr)
    )
    '( ;
      ; 最終的にgotoするアドレスを指定
      (assign continue (label done))
      ; countを初期化
      (assign count (const 0))

      count-leaves
      ; treeがnullならcountは変化しない
      (test (op null?) (reg tree))
      (branch (label immediate-zero))

      ; treeがペアでないならcountは+1
      (assign tmp (op pair?) (reg tree))
      (test (op not) (reg tmp))
      (branch (label immediate-one))

      ; treeがペア
      ;; carについてのcount-leavesの再帰呼出し
      (save tree)
      (assign tree (op car) (reg tree))
      (save continue)
      (assign continue (label count-leaves-for-car-done))
      (goto (label count-leaves))

      count-leaves-for-car-done
      (restore continue)
      (restore tree)

      ;; cdrについてのcount-leavesの再帰呼出し
      (save tree)
      (assign tree (op cdr) (reg tree))
      (save continue)
      (assign continue (label count-leaves-for-cdr-done))
      (goto (label count-leaves))

      count-leaves-for-cdr-done
      (restore continue)
      (restore tree)
      (goto (reg continue))

      immediate-zero
      (goto (reg continue))

      immediate-one
      (assign count (op +) (reg count) (const 1))
      (goto (reg continue))

      done
     )
  )
)

(set-register-contents! 
  machine-b
  'tree
  '(1
    2
    3
    (4 5)
    ((6 7 8 9) 10)
    (11 (12 (13 14 15) 16 17) 18 (19 20) 21 22 23)
    24
    (25 (26 27 28 29) (30) ((31 32 33) 34 (35 36) 37 ((38 39) (40) (41 42))))
   )
)
(start machine-b)
(get-register-contents machine-b 'count)
; 42
