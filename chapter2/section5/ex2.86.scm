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

(define 
  (apply-generic op . args)

  (define raise-failed 'raise-failed)
  (define 
    (raise x)
    (cond 
      ((eq? (type-tag x) 'scheme-number)
       ((get-coercion 'scheme-number 'rational) x)
      )
      ((eq? (type-tag x) 'rational) ((get-coercion 'rational 'complex) x))
      (else raise-failed)
    )
  )
  (define 
    (raise-to datum type)
    (if (eq? (type-tag datum) type) 
      datum
      (let 
        ( ;
         (raised (raise datum))
        )

        (if (eq? raised raise-failed) 
          raise-failed
          (raise-to raised type)
        )
      )
    )
  )

  (define 
    (project x)

    (define 
      (to-number x)
      (cond 
        ((eq? (type-tag x) 'rational)
         (/ (numer x) (denom x))
        )
        ((eq? (type-tag x) 'scheme-number)
         (contents x)
        )
        (else (error "Bad type: to-number" (list x)))
      )
    )

    (cond 
      ((eq? (type-tag x) 'complex)
       (make-rational (to-number (real-part x)) 1)
      )
      ((eq? (type-tag x) 'rational)
       (make-scheme-number (/ (numer x) (denom x)))
      )
      (else x)
    )
  )
  (define 
    (drop x)
    (if (not (pair? x)) 
      x
      (let 
        ( ;
         (projected (project x))
        )

        (cond 
          ((eq? (type-tag x) (type-tag projected)) x)
          ((equ? x projected) (drop projected))
          (else x)
        )
      )
    )
  )

  (let 
    ( ;
     (type-tags (map type-tag args))
    )

    (let 
      ( ;
       (proc (get op type-tags))
      )

      (if proc 
        (drop (apply proc (map contents args)))
        (if (= (length args) 2) 
          (let 
            ( ;
             (type1 (car type-tags))
             (type2 (cadr type-tags))
             (a1 (car args))
             (a2 (cadr args))
            )

            (let 
              ( ;
               (a1-raised (raise-to a1 (type-tag a2)))
               (a2-raised (raise-to a2 (type-tag a1)))
              )

              (cond 
                ((not (eq? a1-raised raise-failed))
                 (apply-generic op a1-raised a2)
                )
                ((not (eq? a2-raised raise-failed))
                 (apply-generic op a1 a2-raised)
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
  (put 
    'equ?
    '(scheme-number scheme-number)
    (lambda (x y) (= x y))
  )
  (put 
    'sine
    '(scheme-number)
    (lambda (x) (tag (sin x)))
  )
  (put 
    'cosine
    '(scheme-number)
    (lambda (x) (tag (cos x)))
  )
  (put 
    'arc-tangent
    '(scheme-number scheme-number)
    (lambda (x y) (tag (atan x y)))
  )
  (put 
    'square-root
    '(scheme-number)
    (lambda (x) (tag (sqrt x)))
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
  (define 
    (equ?-rat x y)
    (= 
      (* (numer x) (denom y))
      (* (denom x) (numer y))
    )
  )
  (define 
    (sine-rat x)
    (make-rat 
      (sin 
        (/ (numer x) (denom x))
      )
      1
    )
  )
  (define 
    (cosine-rat x)
    (make-rat 
      (cos 
        (/ (numer x) (denom x))
      )
      1
    )
  )
  (define 
    (arc-tangent-rat x y)
    (make-rat 
      (atan 
        (/ (numer x) (denom x))
        (/ (numer y) (denom y))
      )
      1
    )
  )
  (define 
    (square-root-rat x)
    (make-rat 
      (sqrt (numer x))
      (sqrt (denom x))
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
    'equ?
    '(rational rational)
    (lambda (x y) (equ?-rat x y))
  )
  (put 
    'sine
    '(rational)
    (lambda (x) (tag (sine-rat x)))
  )
  (put 
    'cosine
    '(rational)
    (lambda (x) (tag (cosine-rat x)))
  )
  (put 
    'arc-tangent
    '(rational rational)
    (lambda (x y) (tag (arc-tangent-rat x y)))
  )
  (put 
    'square-root
    '(rational)
    (lambda (x) (tag (square-root-rat x)))
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
  (define (square x) (mul x x))

  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define 
    (magnitude z)
    (square-root 
      (add 
        (square (real-part z))
        (square (imag-part z))
      )
    )
  )
  (define 
    (angle z)
    (arc-tangent (imag-part z) (real-part z))
  )
  (define 
    (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (cosine a)))
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
  (define (square x) (mul x x))

  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z) (mul (magnitude z) (sine (angle z))))
  (define 
    (make-from-real-imag x y)
    (cons (square-root (add (square x) (square y))) 
          (arc-tangent y x)
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
      (add (real-part z1) (real-part z2))
      (add (imag-part z1) (imag-part z2))
    )
  )
  (define 
    (sub-complex z1 z2)
    (make-from-real-imag 
      (sub (real-part z1) (real-part z2))
      (sub (imag-part z1) (imag-part z2))
    )
  )
  (define 
    (mul-complex z1 z2)
    (make-from-mag-ang 
      (mul (magnitude z1) (magnitude z2))
      (add (angle z1) (angle z2))
    )
  )
  (define 
    (div-complex z1 z2)
    (make-from-mag-ang 
      (div (magnitude z1) (magnitude z2))
      (sub (angle z1) (angle z2))
    )
  )
  (define 
    (equ?-complex z1 z2)
    (and 
      (equ? (magnitude z1) (magnitude z2))
      (equ? (angle z1) (angle z2))
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
    'equ?
    '(complex complex)
    (lambda (z1 z2) (equ?-complex z1 z2))
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
  'rational
  (lambda (x) (make-rational (contents x) 1))
)
(put-coercion 
  'rational
  'complex
  (lambda (x) 
    (make-complex-from-mag-ang 
      (make-scheme-number (/ (numer x) (denom x)))
      (make-scheme-number 0)
    )
  )
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

(define (equ? x y) (apply-generic 'equ? x y))

(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (arc-tangent x y) (apply-generic 'arc-tangent x y))

(define (square-root x) (apply-generic 'square-root x))


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

(define 
  z1
  (make-complex-from-real-imag (make-rational 8 2) (make-scheme-number 3))
)
(real-part z1)
; '(scheme-number . 4)
(imag-part z1)
; '(scheme-number . 3)
(magnitude z1)
; '(scheme-number . 5)
(angle z1)
; '(scheme-number . 0.6435011087932844)

(define 
  z2
  (make-complex-from-mag-ang 
    (make-scheme-number 1.414213562373095048801688724209698)
    (make-rational 3.1415926535897932384626433832795 4)
  )
)
(real-part z2)
; '(scheme-number . 1.0000000000000002)
(imag-part z2)
; '(scheme-number . 1.0)
(magnitude z2)
; '(scheme-number . 1.4142135623730951)
(angle z2)
; '(scheme-number . 0.7853981633974483)

(add z1 z2)
; '(complex rectangular (scheme-number . 5.0) scheme-number . 4.0)
(sub z1 z2)
; '(complex rectangular (scheme-number . 3.0) scheme-number . 2.0)
(mul z1 z2)
; '(complex polar (scheme-number . 7.0710678118654755) scheme-number . 1.4288992721907325)
(div z1 z2)
; '(complex polar (scheme-number . 3.5355339059327373) scheme-number . -0.1418970546041639)
(equ? z1 z1)
; #t
(equ? z1 z2)
; #f
