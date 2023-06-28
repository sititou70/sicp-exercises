#lang racket
(provide (all-defined-out))

(require sicp)
(require "machine/make-machine.rkt")

; main
(define 
  expt-rec-machine
  (make-machine 
    '(b n val continue)
    (list 
      (list '= =)
      (list '- -)
      (list '* *)
    )
    '(controller
      ; 最終的なリターンアドレス
      (assign continue (label expt-done))

      expt-loop
      (test (op =) (reg n) (const 0))
      (branch (label base-case))

      ; 再帰呼出しのセットアップ
      ;; レジスタをsave
      (save continue)
      (save n)
      ;; 呼び出し先のためにレジスタをセット
      (assign n (op -) (reg n) (const 1)) ; n - 1のケースについて計算させる
      (assign continue (label after-expt)) ; 計算が完了したらafter-exptにリターンさせる
      ; 再帰呼出し実行
      (goto (label expt-loop))

      after-expt
      ; レジスタをrestore
      (restore n)
      (restore continue)
      ; 継続処理：最終的なvalを、再帰呼出しの結果であるvalを使用して計算する
      (assign val (op *) (reg b) (reg val))
      ; 呼び出し元に戻る
      (goto (reg continue))

      base-case
      ; 基底ケース：b^0 = 1
      (assign val (const 1))
      ; 呼び出し元に戻る
      (goto (reg continue))

      expt-done
     )
  )
)
(set-register-contents! expt-rec-machine 'b 2)
(set-register-contents! expt-rec-machine 'n 10)
(start expt-rec-machine)
(get-register-contents expt-rec-machine 'val)
; 1024

(define 
  expt-iter-machine
  (make-machine 
    '(b n counter product)
    (list 
      (list '= =)
      (list '- -)
      (list '* *)
    )
    '(controller
      ; レジスタの初期化
      (assign counter (reg n))
      (assign product (const 1))

      expt-loop
      (test (op =) (reg counter) (const 0))
      (branch (label expt-done))

      (assign counter (op -) (reg counter) (const 1))
      (assign product (op *) (reg b) (reg product))
      (goto (label expt-loop))

      expt-done
     )
  )
)
(set-register-contents! expt-iter-machine 'b 2)
(set-register-contents! expt-iter-machine 'n 10)
(start expt-iter-machine)
(get-register-contents expt-iter-machine 'product)
; 1024
