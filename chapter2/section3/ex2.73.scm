#lang racket/base

; see: http://community.schemewiki.org/?sicp-ex-2.73
(define *procedures* (make-hash))
(define (put key1 key2 value) (hash-set! *procedures* (list key1 key2) value))
(define (get key1 key2) (hash-ref *procedures* (list key1 key2) #f))

; a. 下記の微分プログラムは、各項の演算子をタイプタグ、被演算子をデータとして捉えることで、データ主導スタイルで書かれている。
; ここで、number?やvariable?は演算子をもたない（操作を持たない）ため、データ主導スタイルでは記述できない
(define 
  (deriv exp var)

  (define (operator exp) (car exp))
  (define (operands exp) (cdr exp))

  (define 
    (variable? x)
    (symbol? x)
  )
  (define 
    (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2))
  )

  (cond 
    ((number? exp) 0)
    ((variable? exp) (if (same-variable? exp var) 1 0))
    (else
     ((get 'deriv (operator exp)) 
       (operands exp)
       var
     )
    )
  )
)

; b. 和と積に対するコードを追加する
(define 
  (install-deriv-sum)

  (define 
    (=number? exp num)
    (and (number? exp) (= exp num))
  )

  (define 
    (make-sum a1 a2)
    (cond 
      ((=number? a1 0) a2)
      ((=number? a2 0) a1)
      ((and (number? a1) (number? a2))
       (+ a1 a2)
      )
      (else (list '+ a1 a2))
    )
  )
  (define 
    (addend operands)
    (car operands)
  )
  (define 
    (augend operands)
    (cadr operands)
  )

  (define 
    (deriv-sum operands var)
    (make-sum 
      (deriv (addend operands) var)
      (deriv (augend operands) var)
    )
  )

  (put 'make '+ make-sum)
  (put 'deriv '+ deriv-sum)
)

(define 
  (install-deriv-product)

  (define 
    (=number? exp num)
    (and (number? exp) (= exp num))
  )

  (define 
    (make-product m1 m2)
    (cond 
      ((or (=number? m1 0) (=number? m2 0)) 0)
      ((=number? m1 1) m2)
      ((=number? m2 1) m1)
      ((and (number? m1) (number? m2)) (* m1 m2))
      (else (list '* m1 m2))
    )
  )
  (define 
    (multiplier p)
    (car p)
  )
  (define 
    (multiplicand p)
    (cadr p)
  )

  (define 
    (deriv-product operands var)
    ((get 'make '+) 
      (make-product 
        (multiplier operands)
        (deriv (multiplicand operands) var)
      )
      (make-product 
        (deriv (multiplier operands) var)
        (multiplicand operands)
      )
    )
  )

  (put 'make '* make-product)
  (put 'deriv '* deriv-product)
)

(install-deriv-sum)
(deriv 1 'x)
; 0
(deriv 'a 'x)
; 0
(deriv 'x 'x)
; 1
(deriv '(+ x x) 'x)
; 2

(install-deriv-product)
(deriv '(* x y) 'x)
; y
(deriv '(* (* x y) (+ x 3)) 'x)
; (+ (* x y) (* y (+ x 3)))

; c. さらに指数の微分規則を組み込む
(define 
  (install-deriv-exponentiation)

  (define 
    (=number? exp num)
    (and (number? exp) (= exp num))
  )

  (define 
    (make-exponentiation base exponent)
    (cond 
      ((=number? exponent 0) 1)
      ((=number? exponent 1) base)
      (else (list '** base exponent))
    )
  )
  (define 
    (base exp)
    (car exp)
  )
  (define 
    (exponent exp)
    (cadr exp)
  )

  (define 
    (deriv-exponentiation operands var)
    ((get 'make '*) 
      (exponent operands)
      ((get 'make '*) 
        (make-exponentiation (base operands) (- (exponent operands) 1))
        (deriv (base operands) var)
      )
    )
  )

  (put 'make '** make-exponentiation)
  (put 'deriv '** deriv-exponentiation)
)

(install-deriv-exponentiation)
(deriv '(** x 2) 'x)
; '(* 2 x)
(deriv '(** x 3) 'x)
; '(* 3 (** x 2))
(deriv '(** x 1) 'x)
; 1
(deriv '(** x 0) 'x)
; 0
(deriv '(** y 2) 'x)
; 0

; d. 上記のputに指定しているkeyの順序も合わせて変更する必要がある
