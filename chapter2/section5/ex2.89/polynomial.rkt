#lang racket/base

(require "data-directed-utils.rkt")
(require "generic-procs.rkt")

; --------
; 多項式パッケージ
; --------

; タワーの中で最も低い型で数値を作成するコンストラクタ
; 新しい型を追加する場合は合わせて変更する
(define (make-bottom-number x) (make-scheme-number x))

(define 
  (install-polynomial-package)
  ;; 内部手続き
  ;; poly の表現
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ;; 項リストの表現
  (define 
    (first-term term-list)
    (cons 
      (length (cdr term-list)) ; order
      (car term-list) ; coeff
    )
  )
  (define (rest-terms term-list) (cdr term-list))

  (define 
    (adjoin-term order coeff term-list)
    (cond 
      ((=zero? coeff)
       term-list
      )
      ((= order (length term-list))
       (cons coeff term-list)
      )
      (else
       (adjoin-term 
         order
         coeff
         (cons (make-bottom-number 0) term-list)
       )
      )
    )
  )

  ;; 演算
  (define (order term) (car term))
  (define (coeff term) (cdr term))

  (define (the-empty-termlist) '())
  (define (empty-termlist? term-list) (null? term-list))

  (define 
    (=zero?-termlist term-list)
    (if (empty-termlist? term-list) 
      #t
      (and 
        (=zero? (coeff (first-term term-list)))
        (=zero?-termlist (rest-terms term-list))
      )
    )
  )
  (define 
    (=zero?-poly p)
    (=zero?-termlist (term-list p))
  )

  (define 
    (add-terms L1 L2)

    (cond 
      ((empty-termlist? L1) L2)
      ((empty-termlist? L2) L1)
      (else
       (let 
         ( ;
          (t1 (first-term L1))
          (t2 (first-term L2))
         )

         (cond 
           ((> (order t1) (order t2))
            (adjoin-term 
              (order t1)
              (coeff t1)
              (add-terms (rest-terms L1) L2)
            )
           )
           ((< (order t1) (order t2))
            (adjoin-term 
              (order t2)
              (coeff t2)
              (add-terms L1 (rest-terms L2))
            )
           )
           (else
            (adjoin-term 
              (order t1)
              (add (coeff t1) (coeff t2))
              (add-terms 
                (rest-terms L1)
                (rest-terms L2)
              )
            )
           )
         )
       )
      )
    )
  )

  (define 
    (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2)) 
      (make-poly 
        (variable p1)
        (add-terms (term-list p1) (term-list p2))
      )
      (error "Polys not in same var: ADD-POLY " (list p1 p2))
    )
  )

  (define 
    (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L) 
      (the-empty-termlist)
      (let 
        ( ;
         (t2 (first-term L))
        )

        (adjoin-term 
          (+ (order t1) (order t2))
          (mul (coeff t1) (coeff t2))
          (mul-term-by-all-terms t1 (rest-terms L))
        )
      )
    )
  )

  (define 
    (mul-terms L1 L2)
    (if (empty-termlist? L1) 
      (the-empty-termlist)
      (add-terms 
        (mul-term-by-all-terms (first-term L1) L2)
        (mul-terms (rest-terms L1) L2)
      )
    )
  )

  (define 
    (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2)) 
      (make-poly 
        (variable p1)
        (mul-terms (term-list p1) (term-list p2))
      )
      (error "Polys not in same var: MUL-POLY " (list p1 p2))
    )
  )

  ;; システムのほかの部分とのインターフェイス
  (define (tag p) (attach-tag 'polynomial p))
  (put 
    'add
    '(polynomial polynomial)
    (lambda (p1 p2) (tag (add-poly p1 p2)))
  )
  (put 
    'mul
    '(polynomial polynomial)
    (lambda (p1 p2) (tag (mul-poly p1 p2)))
  )
  (put 
    '=zero?
    '(polynomial)
    (lambda (p) (=zero?-poly p))
  )
  (put 
    'make
    'polynomial
    (lambda (var terms) (tag (make-poly var terms)))
  )
  'done
)
(install-polynomial-package)
