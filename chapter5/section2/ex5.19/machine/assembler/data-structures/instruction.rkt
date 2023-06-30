#lang racket
(provide (all-defined-out))

(require sicp)

; 命令：マシンが実行する処理の最小単位
(define 
  (make-instruction text)
  (cons 
    (cons text '())
    (list '() '() false)
  )
)
;; 命令のテキスト：マシンの実行には必要ないが、デバッグのためにあると便利
(define 
  (instruction-text inst)
  (caar inst)
)
;; 命令を実行する手続き
(define 
  (instruction-execution-proc inst)
  (cdar inst)
)
(define 
  (set-instruction-execution-proc! inst proc)
  (set-car! inst (cons (instruction-text inst) proc))
)
;; 位置とブレーク
;;; 直前のラベル
(define 
  (instruction-label inst)
  (cadr inst)
)
(define 
  (set-instruction-label! inst label-name)
  (set-cdr! 
    inst
    (list 
      label-name
      (caddr inst)
      (cadddr inst)
    )
  )
)
;;; 直前のラベルからの位置
(define 
  (instruction-position inst)
  (caddr inst)
)
(define 
  (set-instruction-position! inst pos)
  (set-cdr! 
    inst
    (list 
      (cadr inst)
      pos
      (cadddr inst)
    )
  )
)
;;; ブレークポイントかどうか
(define 
  (instruction-break? inst)
  (cadddr inst)
)
(define 
  (set-instruction-break! inst break?)
  (set-cdr! 
    inst
    (list 
      (cadr inst)
      (caddr inst)
      break?
    )
  )
)
