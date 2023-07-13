#lang racket
(provide (all-defined-out))

(require sicp)

; 命令：マシンが実行する処理の最小単位
(define 
  (make-instruction text)
  (cons text '())
)
;; 命令のテキスト：マシンの実行には必要ないが、デバッグのためにあると便利
(define 
  (instruction-text inst)
  (car inst)
)
;; 命令を実行する手続き
(define 
  (instruction-execution-proc inst)
  (cdr inst)
)
(define 
  (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc)
)
