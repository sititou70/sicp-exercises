#lang racket/base

(require "data-directed-utils.rkt")
(require "generic-procs.rkt")

; --------
; 多項式パッケージ
; --------

(define 
  (install-polynomial-package)

  (define (tag p) (attach-tag 'polynomial p))

  ;; 内部手続き
  ;; poly の表現
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (poly? p) (eq? (type-tag p) 'polynomial))

  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ;; 項と項リストの表現
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define empty-termlist '())
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
    (make-poly 
      (variable p1)
      (add-terms 
        (term-list p1)
        (term-list (transform-poly-for-var p2 (variable p1)))
      )
    )
  )

  (define 
    (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L) 
      empty-termlist
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
      empty-termlist
      (add-terms 
        (mul-term-by-all-terms (first-term L1) L2)
        (mul-terms (rest-terms L1) L2)
      )
    )
  )

  (define 
    (mul-poly p1 p2)
    (make-poly 
      (variable p1)
      (mul-terms 
        (term-list p1)
        (term-list (transform-poly-for-var p2 (variable p1)))
      )
    )
  )

  (define 
    (transform-poly-for-var poly target-var)

    (define orig-var (variable poly))

    (define 
      (iter result orig-terms)

      (if (null? orig-terms) 
        result
        (let 
          ( ;
           (term (first-term orig-terms))
          )

          (if (poly? (coeff term)) 
            (let 
              ( ;
               (transformed-poly-for-target-var 
                 (foldl 
                   add-poly
                   (make-poly target-var empty-termlist)
                   (map 
                     (lambda (target-term) 
                       (make-poly 
                         target-var
                         (list 
                           (make-term 
                             (order target-term)
                             (tag 
                               (make-poly 
                                 orig-var
                                 (list (make-term (order term) (coeff target-term)))
                               )
                             )
                           )
                         )
                       )
                     )
                     (term-list 
                       (transform-poly-for-var (contents (coeff term)) target-var)
                     )
                   )
                 )
               )
              )

              (iter 
                (add-poly transformed-poly-for-target-var result)
                (rest-terms orig-terms)
              )
            )

            (iter 
              (add-poly 
                (make-poly 
                  target-var
                  (list 
                    (make-term 
                      0
                      (tag (make-poly orig-var (list term)))
                    )
                  )
                )
                result
              )
              (rest-terms orig-terms)
            )
          )
        )
      )
    )

    (if (same-variable? (variable poly) target-var) 
      poly
      (iter (make-poly target-var empty-termlist) (term-list poly))
    )
  )

  ;; システムのほかの部分とのインターフェイス
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
