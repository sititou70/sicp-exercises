#lang racket/base

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

; 一般の場合に対応するapply-generic
; 複数のサブタイプ、複数のスーパータイプ、型強制変換関係のループに対応する
(define 
  (apply-generic op . args)

  (define 
    ; あるデータから推移的に型変換可能なすべてのデータを探索する
    (search-all-coercions datum)

    (define 
      (next-types type)
      (define coercion-keys (hash-keys *coercions*))
      (map 
        cadr
        (filter 
          (lambda (key) (eq? (car key) type))
          coercion-keys
        )
      )
    )

    (define 
      (filter-by-type data)
      (if (null? data) 
        data
        (if 
          (findf 
            (lambda (item) 
              (eq? 
                (type-tag (car data))
                (type-tag item)
              )
            )
            (cdr data)
          )
          (filter-by-type (cdr data))
          (cons (car data) (filter-by-type (cdr data)))
        )
      )
    )

    (define 
      (search datum found-data)

      (let 
        ( ;
         (target-types 
           (filter 
             (lambda (type) 
               (not 
                 (memf 
                   (lambda (found-datum) (eq? type (type-tag found-datum)))
                   found-data
                 )
               )
             )
             (next-types (type-tag datum))
           )
         )
        )

        (let 
          (
           ;
           (search-results 
             (map 
               (lambda (type) 
                 (let 
                   ( ;
                    (coercioned-datum 
                      ((get-coercion (type-tag datum) type) datum)
                    )
                   )

                   (search coercioned-datum (cons coercioned-datum found-data))
                 )
               )
               target-types
             )
           )
          )

          (filter-by-type (foldl append found-data search-results))
        )
      )
    )

    (search datum (list datum))
  )

  (define search-failed 'search-failed)

  (define 
    (search-application fixed-args args)
    (let 
      ( ;
       (proc 
         (get 
           op
           (map 
             type-tag
             (append fixed-args args)
           )
         )
       )
      )

      (cond 
        (proc (apply proc (map contents (append fixed-args args))))
        ((= (length args) 0) search-failed)
        (else
         (let 
           ( ;
            (current-arg-coercions (search-all-coercions (car args)))
            (args (cdr args))
           )

           (let 
             ((result 
                (findf 
                  (lambda (result) (not (eq? result search-failed)))
                  (append 
                    (map 
                      (lambda (arg) 
                        (search-application 
                          (append fixed-args (list arg))
                          args
                        )
                      )
                      current-arg-coercions
                    )
                    (list 'find-failed)
                  )
                )
              ) 
             )

             (if (eq? result 'find-failed) search-failed result)
           )
         )
        )
      )
    )
  )

  (let 
    ( ;
     (result (search-application '() args))
    )

    (if (eq? result search-failed) 
      (error 
        "No method for these types "
        (map type-tag args)
      )
      result
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

  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)

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
(put-coercion 
  'scheme-number
  'scheme-number
  (lambda (x) x)
)
(put-coercion 
  'scheme-number
  'rational
  (lambda (x) (make-rational (contents x) 1))
)
(put-coercion 
  'rational
  'scheme-number
  (lambda (x) (make-scheme-number (/ (numer x) (denom x))))
)
(put-coercion 
  'rational
  'complex
  (lambda (x) (make-complex-from-mag-ang (/ (numer x) (denom x)) 0))
)

; --------
; ジェネリック演算
; --------
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (numer x) (apply-generic 'numer x))
(define (denom x) (apply-generic 'denom x))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define 
  (make-scheme-number n)
  ((get 'make 'scheme-number) n)
)
(define 
  (make-rational n d)
  ((get 'make 'rational) n d)
)
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

; ('rational 'complex 'rational)をとって、第2引数の絶対値が第1引数より大きく第3引数より小さいかを判定する
(define (rat-comp-range x y z) (apply-generic 'rat-comp-range x y z))
(put 
  'rat-comp-range
  '(rational complex rational)
  (lambda (x y z) 
    (< (/ (car x) (cdr x)) (magnitude y) (/ (car z) (cdr z)))
  )
)

(rat-comp-range 
  (make-rational 1 1)
  (make-complex-from-mag-ang 2 123)
  (make-rational 3 1)
)
; #t
(rat-comp-range 
  (make-scheme-number 1)
  (make-complex-from-mag-ang 2 123)
  (make-rational 3 1)
)
; #t
(rat-comp-range 
  (make-rational 1 1)
  (make-complex-from-mag-ang 2 123)
  (make-scheme-number 3)
)
; #t
(rat-comp-range 
  (make-scheme-number 1)
  (make-scheme-number 2)
  (make-scheme-number 3)
)
; #t
(rat-comp-range 
  (make-scheme-number 1)
  (make-scheme-number 2)
  (make-complex-from-mag-ang 3 123)
)
; No method for these types  '(scheme-number scheme-number complex)

; 戦略のひとつとしては、すべての引数を一つ目の引数の型に強制型変換することを試み、
; 次に二つ目の引数の型に強制型変換することを試み、ということを続けるというものだ。
; この戦略について (また、上で述べた二引数バージョンについても)、それが十分に一般的でないような状況の例を挙げよ

; 上記の戦略は一般的ではない。この戦略はすべての引数の型を同じにしようとするため、異なる引数の型に対する演算ではうまくいかない
; hoge演算は'(ratilnal complex)の引数を取る。ここで'(scheme-number complex)の引数でhogeを呼び出すとき、
; 例えばscheme-number->complexの型強制が実装されていれば、上記の戦略では'(complex complex)に型を強制してしまい、演算を適用できない。
