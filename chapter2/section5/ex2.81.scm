#lang racket/base
(require racket/trace)

; --------
; データ主導スタイルのためのユーティリティ
; --------
(define *procedures* (make-hash))
(define (put key1 key2 value) (hash-set! *procedures* (list key1 key2) value))
(define (get key1 key2) (hash-ref *procedures* (list key1 key2) #f))

(define *coercions* (make-hash))
(define 
  (put-coercion key1 key2 value)
  (hash-set! *coercions* (list key1 key2) value)
)
(define (get-coercion key1 key2) (hash-ref *coercions* (list key1 key2) #f))

(define 
  (attach-tag type-tag contents)
  (cons type-tag contents)
)
(define 
  (type-tag datum)
  (if (pair? datum) 
    (car datum)
    (error "Bad tagged datum : TYPE-TAG " datum)
  )
)
(define 
  (contents datum)
  (if (pair? datum) 
    (cdr datum)
    (error "Bad tagged datum : CONTENTS " datum)
  )
)

(define 
  (apply-generic op . args)
  (let 
    ((type-tags (map type-tag args)))
    (let 
      ((proc (get op type-tags)))
      (if proc 
        (apply proc (map contents args))
        (if (= (length args) 2) 
          (let 
            ((type1 (car type-tags)) 
              (type2 (cadr type-tags))
              (a1 (car args))
              (a2 (cadr args))
            )
            (let 
              ((t1->t2 (get-coercion type1 type2)) 
                (t2->t1 (get-coercion type2 type1))
              )
              (cond 
                (t1->t2
                 (apply-generic op (t1->t2 a1) a2)
                )
                (t2->t1
                 (apply-generic op a1 (t2->t1 a2))
                )
                (else
                 (error 
                   "No method for these types "
                   (list op type-tags)
                 )
                )
              )
            )
          )
          (error 
            "No method for these types "
            (list op type-tags)
          )
        )
      )
    )
  )
)

(define 
  (apply-generic-fixed op . args)
  (let 
    ((type-tags (map type-tag args)))
    (let 
      ((proc (get op type-tags)))
      (if proc 
        (apply proc (map contents args))
        (if 
          (and (= (length args) 2) 
               (not (andmap (lambda (type) (eq? type (car type-tags))) type-tags))
          )
          (let 
            ((type1 (car type-tags)) 
              (type2 (cadr type-tags))
              (a1 (car args))
              (a2 (cadr args))
            )
            (let 
              ((t1->t2 (get-coercion type1 type2)) 
                (t2->t1 (get-coercion type2 type1))
              )
              (cond 
                (t1->t2
                 (apply-generic op (t1->t2 a1) a2)
                )
                (t2->t1
                 (apply-generic op a1 (t2->t1 a2))
                )
                (else
                 (error 
                   "No method for these types "
                   (list op type-tags)
                 )
                )
              )
            )
          )
          (error 
            "No method for these types "
            (list op type-tags)
          )
        )
      )
    )
  )
)

; --------
; 通常の算術演算パッケージ
; --------
(define 
  (install-scheme-number-package)

  (define (tag x) (attach-tag 'scheme-number x))
  (put 
    'add
    '(scheme-number scheme-number)
    (lambda (x y) (tag (+ x y)))
  )
  (put 
    'sub
    '(scheme-number scheme-number)
    (lambda (x y) (tag (- x y)))
  )
  (put 
    'mul
    '(scheme-number scheme-number)
    (lambda (x y) (tag (* x y)))
  )
  (put 
    'div
    '(scheme-number scheme-number)
    (lambda (x y) (tag (/ x y)))
  )
  ;; 以下は Scheme-number パッケージに追加する
  (put 
    'exp
    '(scheme-number scheme-number)
    (lambda (x y) (tag (expt x y))) ; 基本手続き expt を使う
  )
  (put 'make 'scheme-number (lambda (x) (tag x)))

  'done
)

; --------
; 有理数算術演算パッケージ
; --------
(define 
  (install-rational-package)

  ;; 内部手続き
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define 
    (make-rat n d)
    (let 
      ((g (gcd n d)))
      (cons (/ n g) (/ d g))
    )
  )
  (define 
    (add-rat x y)
    (make-rat 
      (+ (* (numer x) (denom y)) 
         (* (numer y) (denom x))
      )
      (* (denom x) (denom y))
    )
  )
  (define 
    (sub-rat x y)
    (make-rat 
      (- (* (numer x) (denom y)) 
         (* (numer y) (denom x))
      )
      (* (denom x) (denom y))
    )
  )
  (define 
    (mul-rat x y)
    (make-rat 
      (* (numer x) (numer y))
      (* (denom x) (denom y))
    )
  )
  (define 
    (div-rat x y)
    (make-rat 
      (* (numer x) (denom y))
      (* (denom x) (numer y))
    )
  )

  ;; システムのほかの部分とのインターフェイス
  (define (tag x) (attach-tag 'rational x))

  (put 
    'add
    '(rational rational)
    (lambda (x y) (tag (add-rat x y)))
  )
  (put 
    'sub
    '(rational rational)
    (lambda (x y) (tag (sub-rat x y)))
  )
  (put 
    'mul
    '(rational rational)
    (lambda (x y) (tag (mul-rat x y)))
  )
  (put 
    'div
    '(rational rational)
    (lambda (x y) (tag (div-rat x y)))
  )
  (put 
    'make
    'rational
    (lambda (n d) (tag (make-rat n d)))
  )

  'done
)

; --------
; 複素数算術演算パッケージ
; --------

; 直交形式パッケージ
(define 
  (install-rectangular-package)

  ;; 内部手続き
  (define (square x) (* x x))

  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define 
    (magnitude z)
    (sqrt 
      (+ (square (real-part z)) 
         (square (imag-part z))
      )
    )
  )
  (define 
    (angle z)
    (atan (imag-part z) (real-part z))
  )
  (define 
    (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a)))
  )

  ;; システムのほかの部分とのインターフェイス
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 
    'make-from-real-imag
    'rectangular
    (lambda (x y) (tag (make-from-real-imag x y)))
  )
  (put 
    'make-from-mag-ang
    'rectangular
    (lambda (r a) (tag (make-from-mag-ang r a)))
  )

  'done
)

; 極座標パッケージ
(define 
  (install-polar-package)

  ;; 内部手続き
  (define (square x) (* x x))

  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define 
    (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))) 
          (atan y x)
    )
  )

  ;; システムのほかの部分とのインターフェイス
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 
    'make-from-real-imag
    'polar
    (lambda (x y) (tag (make-from-real-imag x y)))
  )
  (put 
    'make-from-mag-ang
    'polar
    (lambda (r a) (tag (make-from-mag-ang r a)))
  )

  'done
)

; 複素数パッケージ
(define 
  (install-complex-package)

  (install-rectangular-package)
  (install-polar-package)

  ;; 直交形式パッケージと極形式パッケージからインポートした手続き
  (define 
    (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y)
  )
  (define 
    (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a)
  )

  ;; 内部手続き
  (define 
    (add-complex z1 z2)
    (make-from-real-imag 
      (+ (real-part z1) (real-part z2))
      (+ (imag-part z1) (imag-part z2))
    )
  )
  (define 
    (sub-complex z1 z2)
    (make-from-real-imag 
      (- (real-part z1) (real-part z2))
      (- (imag-part z1) (imag-part z2))
    )
  )
  (define 
    (mul-complex z1 z2)
    (make-from-mag-ang 
      (* (magnitude z1) (magnitude z2))
      (+ (angle z1) (angle z2))
    )
  )
  (define 
    (div-complex z1 z2)
    (make-from-mag-ang 
      (/ (magnitude z1) (magnitude z2))
      (- (angle z1) (angle z2))
    )
  )

  ;; システムのほかの部分とのインターフェイス
  (define (tag z) (attach-tag 'complex z))
  (put 
    'add
    '(complex complex)
    (lambda (z1 z2) (tag (add-complex z1 z2)))
  )
  (put 
    'sub
    '(complex complex)
    (lambda (z1 z2) (tag (sub-complex z1 z2)))
  )
  (put 
    'mul
    '(complex complex)
    (lambda (z1 z2) (tag (mul-complex z1 z2)))
  )
  (put 
    'div
    '(complex complex)
    (lambda (z1 z2) (tag (div-complex z1 z2)))
  )
  (put 
    'make-from-real-imag
    'complex
    (lambda (x y) (tag (make-from-real-imag x y)))
  )
  (put 
    'make-from-mag-ang
    'complex
    (lambda (r a) (tag (make-from-mag-ang r a)))
  )

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  'done
)

; --------
; 型強制変換
; --------
(define 
  (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0)
)
(put-coercion 
  'scheme-number
  'complex
  scheme-number->complex
)

; Louisの追加実装
(define (scheme-number->scheme-number n) n)
(put-coercion 
  'scheme-number
  'scheme-number
  scheme-number->scheme-number
)
(define (complex->complex z) z)
(put-coercion 'complex 'complex complex->complex)

; --------
; ジェネリック演算
; --------
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
; scheme-numberに対してのみの演算
(define (exp x y) (apply-generic 'exp x y))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define 
  (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y)
)
(define 
  (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a)
)

; --------
; main
; --------
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

(define z '(complex rectangular 3 . 4))

; a. 以下は停止しない。第1引数のcomplexを第2引数のcomplex型に型変換し続けてしまう
; (exp z z)

; b. ある型を自身の型に強制変換するという考えは一般的に不要である。Louisの書いた型変換のコードは不要に見える。
; 一方で、同じ型に対する型変換がテーブルにあるとき、ハングアップしてしまうapply-genericは、課題cで修正されたapply-genericと比べて適切ではないといえる。

; c. 修正版のapply-generic-fixedを実装した。
(define (exp-fixed x y) (apply-generic-fixed 'exp x y))
(exp-fixed '(scheme-number . 2) '(scheme-number . 10))
; '(scheme-number . 1024)
(exp-fixed z z)
; No method for these types  '(exp (complex complex))
