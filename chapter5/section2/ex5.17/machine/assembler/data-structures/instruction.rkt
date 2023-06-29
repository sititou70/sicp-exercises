#lang racket
(provide (all-defined-out))

(require sicp)

; 命令：マシンが実行する処理の最小単位
(define 
  (make-instruction text)
  (cons '() (cons text '()))
)
;; 命令のラベル：その命令の直前のラベル
(define 
  (instruction-label inst)
  (car inst)
)
(define 
  (set-instruction-label! inst label-name)
  (set-car! inst label-name)
)
;; 命令のテキスト：マシンの実行には必要ないが、デバッグのためにあると便利
(define 
  (instruction-text inst)
  (cadr inst)
)
;; 命令を実行する手続き
(define 
  (instruction-execution-proc inst)
  (cddr inst)
)
(define 
  (set-instruction-execution-proc! inst proc)
  (set-cdr! (cdr inst) proc)
)
