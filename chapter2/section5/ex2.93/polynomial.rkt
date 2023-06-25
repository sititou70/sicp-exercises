#lang racket/base

(require "data-directed-utils.rkt")
(require "generic-procs.rkt")

; --------
; 多項式パッケージ
; --------

(define 
  (install-polynomial-package)
  ;; 内部手続き
  ;; poly の表現
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ;; 項と項リストの表現
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define 
    (=zero?-term term)
    (=zero? (coeff term))
  )
  (define 
    (=zero?-poly p)
    (andmap =zero?-term (term-list p))
  )

  (define 
    (equ?-poly p1 p2)
    (=zero? (sub p1 p2))
  )

  (define 
    (additive-inverse-term term)
    (make-term (order term) (additive-inverse (coeff term)))
  )
  (define 
    (additive-inverse-poly p)
    (make-poly 
      (variable p)
      (map additive-inverse-term (term-list p))
    )
  )

  (define 
    (adjoin-term term term-list)
    (if (=zero? (coeff term)) 
      term-list
      (cons term term-list)
    )
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
              t1
              (add-terms (rest-terms L1) L2)
            )
           )
           ((< (order t1) (order t2))
            (adjoin-term 
              t2
              (add-terms L1 (rest-terms L2))
            )
           )
           (else
            (adjoin-term 
              (make-term 
                (order t1)
                (add (coeff t1) (coeff t2))
              )
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
    (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2)) 
      (add-poly p1 (additive-inverse-poly p2))
      (error "Polys not in same var: SUB-POLY " (list p1 p2))
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
          (make-term 
            (+ (order t1) (order t2))
            (mul (coeff t1) (coeff t2))
          )
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

  (define 
    (div-terms L1 L2)

    (if (empty-termlist? L1) 
      (list (the-empty-termlist) (the-empty-termlist))
      (let 
        ( ;
         (t1 (first-term L1))
         (t2 (first-term L2))
        )

        (if (> (order t2) (order t1)) 
          (list (the-empty-termlist) L1)
          (let 
            ( ;
             (new-c (div (coeff t1) (coeff t2)))
             (new-o (- (order t1) (order t2)))
            )

            (let 
              ( ;
               (rest-of-result 
                 (div-terms 
                   (add-terms 
                     L1
                     (map 
                       additive-inverse-term
                       (mul-term-by-all-terms 
                         (make-term new-o new-c)
                         L2
                       )
                     )
                   )
                   L2
                 )
               )
              )

              (list 
                (adjoin-term (make-term new-o new-c) (car rest-of-result))
                (cadr rest-of-result)
              )
            )
          )
        )
      )
    )
  )

  (define 
    (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2)) 
      (make-poly 
        (variable p1)
        (div-terms (term-list p1) (term-list p2))
      )
      (error "Polys not in same var: DIV-POLY " (list p1 p2))
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
    'sub
    '(polynomial polynomial)
    (lambda (p1 p2) (tag (sub-poly p1 p2)))
  )
  (put 
    'mul
    '(polynomial polynomial)
    (lambda (p1 p2) (tag (mul-poly p1 p2)))
  )
  (put 
    'div
    '(polynomial polynomial)
    (lambda (p1 p2) (map tag (div-poly p1 p2)))
  )
  (put 
    '=zero?
    '(polynomial)
    (lambda (p) (=zero?-poly p))
  )
  (put 
    'equ?
    '(polynomial)
    (lambda (p) (equ?-poly p))
  )
  (put 
    'additive-inverse
    '(polynomial)
    (lambda (p) (tag (additive-inverse-poly p)))
  )
  (put 
    'make
    'polynomial
    (lambda (var terms) (tag (make-poly var terms)))
  )
  'done
)
(install-polynomial-package)
