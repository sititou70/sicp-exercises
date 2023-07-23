#lang racket

(require sicp)
(require "compiler/compile.rkt")
(require "compiler/instruction-sequence.rkt")

; ################
; statements printer
; ################
(define 
  (print-label-prototypes statements)
  (cond 
    ((null? statements) (void))
    ((symbol? (car statements))
     (display "void* ")
     (display (car statements))
     (displayln "(void);")
     (print-label-prototypes (cdr statements))
    )
    (else
     (print-label-prototypes (cdr statements))
    )
  )
)

; 次のラベルへの暗黙的なgotoを明示的に追加する
(define 
  (add-implicit-goto statements)
  (cond 
    ((null? statements) statements)
    ((null? (cdr statements)) statements)
    ; 次がラベル
    ((symbol? (cadr statements))
     (if 
       (or 
         (not (pair? (car statements)))
         (not (eq? (caar statements) 'goto))
       )
       (cons 
         (car statements)
         (cons 
           `
           (goto (label , (cadr statements)))
           (add-implicit-goto (cdr statements))
         )
       )
       (cons 
         (car statements)
         (add-implicit-goto (cdr statements))
       )
     )
    )
    (else
     (cons 
       (car statements)
       (add-implicit-goto (cdr statements))
     )
    )
  )
)

(define 
  (print-all-statements statements)

  (define 
    (iter label current-statements rest-statements)
    (cond 
      ((null? rest-statements)
       (print-statements label current-statements)
       (void)
      )
      ((symbol? (car rest-statements))
       (print-statements label current-statements)
       (iter (car rest-statements) '() (cdr rest-statements))
      )
      (else
       (iter 
         label
         (cons (car rest-statements) current-statements)
         (cdr rest-statements)
       )
      )
    )
  )

  (iter 'compiled_entry '() statements)
)

(define 
  (print-statements label statements)

  (define 
    (iter statements)
    (if (null? statements) 
      (displayln "}\n")
      (begin 
        (display "  ")
        (print-statement (car statements))
        (iter (cdr statements))
      )
    )
  )

  (display "void* ")
  (display label)
  (displayln "(void) {")
  (if (null? statements) 
    (begin 
      (displayln "  return NULL;")
      (displayln "}\n")
    )
    (iter (reverse statements))
  )
)

;; statement
(define 
  (print-statement statement)
  (cond 
    ((eq? (car statement) 'goto)
     (print-goto statement)
    )
    ((eq? (car statement) 'assign)
     (print-assign statement)
    )
    ((eq? (car statement) 'perform)
     (print-perform statement)
    )
    ((eq? (car statement) 'test)
     (print-test statement)
    )
    ((eq? (car statement) 'branch)
     (print-branch statement)
    )
    ((eq? (car statement) 'save)
     (print-save statement)
    )
    ((eq? (car statement) 'restore)
     (print-restore statement)
    )
    (else (displayln statement))
  )
)

(define 
  (print-goto statement)
  (display "goto(")

  (define 
    (register-symbol? sym)
    (cond 
      ((eq? sym 'exp) true)
      ((eq? sym 'env) true)
      ((eq? sym 'val) true)
      ((eq? sym 'proc) true)
      ((eq? sym 'argl) true)
      ((eq? sym 'continue) true)
      ((eq? sym 'unev) true)
      (else false)
    )
  )

  ; operand: (reg ...) | (label ...)
  (define operand (cadr statement))
  (cond 
    ((eq? (car operand) 'reg)
     (display "reg_")
     (if (register-symbol? (cadr operand)) 
       (begin 
         (display (cadr operand))
         (display "->internal_label")
       )
       (display (cadr operand))
     )
    )
    ((eq? (car operand) 'label)
     (display (cadr operand))
    )
    (else (display operand))
  )

  (displayln ");")
)

;; statement: (assign target (op ...) (const ...) (reg ...))
;; statement: (assign target (const ...))
;; statement: (assign target (reg ...))
;; statement: (assign target (label ...))
(define 
  (print-assign statement)
  (define target (cadr statement))
  (define first-operator (caddr statement))
  (define operator-operands (cddr statement))

  (display "assign(reg_")
  (display target)
  (display ", ")

  (if (eq? (car first-operator) 'op) 
    (print-operator-operands operator-operands)
    (print-operand first-operator)
  )

  (displayln ");")
)

(define 
  (print-perform statement)
  (display "perform(")
  (print-operator-operands (cdr statement))
  (displayln ");")
)

(define 
  (print-test statement)
  (display "test(")
  (print-operator-operands (cdr statement))
  (displayln ");")
)

(define 
  (print-branch statement)
  (display "branch(")
  (display (cadadr statement))
  (displayln ");")
)

(define 
  (print-save statement)
  (display "save(reg_")
  (display (cadr statement))
  (displayln ");")
)

(define 
  (print-restore statement)
  (display "restore(reg_")
  (display (cadr statement))
  (displayln ");")
)

;; utils
;;; exp: ((op ...) (...) (...))
(define 
  (print-operator-operands exp)
  (define operator-name (cadar exp))
  (define operands (cdr exp))

  (display operator-name)
  (display "(")
  (print-operand (car operands))
  (for-each 
    (lambda (operand) 
      (display ", ")
      (print-operand operand)
    )
    (cdr operands)
  )
  (display ")")
)

;;; operand: (reg ...) | (const ...) | (label ...)|
(define 
  (print-operand operand)

  (define 
    (print-const-list l)
    (display "list(")
    (print-const (car l))
    (for-each 
      (lambda (item) 
        (display ", ")
        (print-const item)
      )
      (cdr l)
    )
    (display ")")
  )

  (define 
    (print-const val)
    (cond 
      ((number? val)
       (display "make_number(")
       (display val)
       (display ")")
      )
      ((symbol? val)
       (display "make_symbol(\"")
       (display val)
       (display "\")")
      )
      ((null? val)
       (display "make_null()")
      )
      ((pair? val)
       (print-const-list val)
      )
      (else (display val))
    )
  )

  (cond 
    ((eq? (car operand) 'reg)
     (display "reg_")
     (display (cadr operand))
    )
    ((eq? (car operand) 'label)
     (display "make_internal_label(")
     (display (cadr operand))
     (display ")")
    )
    ((eq? (car operand) 'const)
     (print-const (cadr operand))
    )
    (else (display operand))
  )
)

; ################
; io
; ################
(define 
  (print-template-header port)
  (let 
    ( ;
     (line (read-line port))
    )

    (cond 
      ((eof-object? line) (void))
      ((string=? line "// [compiled code here]") (void))
      (else (displayln line) (print-template-header port))
    )
  )
)
(define 
  (print-template-rest port)
  (let 
    ( ;
     (line (read-line port))
    )

    (cond 
      ((eof-object? line) (void))
      (else (displayln line) (print-template-header port))
    )
  )
)

; ################
; main
; ################
;; read source code
(define 
  input-file
  (open-input-file (vector-ref (current-command-line-arguments) 0))
)
;;; racketの#langを読み飛ばす
(void (read-line input-file))
(define source-program (read input-file))

;; compile
(define 
  compiled-statements
  (add-implicit-goto 
    (statements 
      (compile source-program 'val 'return)
    )
  )
)

;; output c code
(define 
  template-file
  (open-input-file "main.template.c")
)
(print-template-header template-file)
(display "// -------- start compiled code --------\n")
(print-label-prototypes compiled-statements)
(display "\n")
(print-all-statements compiled-statements)
(display "// -------- end compiled code --------\n")
(print-template-rest template-file)
