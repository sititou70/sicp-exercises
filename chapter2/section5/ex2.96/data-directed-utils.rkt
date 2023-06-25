#lang racket/base
(provide (all-defined-out))

; --------
; データ主導スタイルのためのユーティリティ
; --------

; 型-手続きテーブル
(define *procedures* (make-hash))
(define (put key1 key2 value) (hash-set! *procedures* (list key1 key2) value))
(define (get key1 key2) (hash-ref *procedures* (list key1 key2) #f))

; 強制型変換テーブル
(define *coercions* (make-hash))
(define 
  (put-coercion key1 key2 value)
  (hash-set! *coercions* (list key1 key2) value)
)
(define (get-coercion key1 key2) (hash-ref *coercions* (list key1 key2) #f))

; 上位型関係テーブル
(define *supertypes* (make-hash))
; 部分型関係テーブル
(define *subtypes* (make-hash))
(define 
  (put-supertype type supertype)
  (hash-set! *supertypes* type supertype)
  (hash-set! *subtypes* supertype type)
  'done
)
(define (get-supertype type) (hash-ref *supertypes* type #f))
(define (get-subtype type) (hash-ref *subtypes* type #f))

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
    (let 
      ( ;
       (type (type-tag x))
       (supertype (get-supertype (type-tag x)))
      )

      (if supertype 
        ((get-coercion type supertype) x)
        raise-failed
      )
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
    (let 
      ( ;
       (type (type-tag x))
       (subtype (get-subtype (type-tag x)))
      )

      (if subtype 
        ((get-coercion type subtype) x)
        x
      )
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

  (define (equ? x y) (apply-generic 'equ? x y))

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
        (if 
          (and (= (length args) 2) 
               (not (eq? (type-tag (car args)) (type-tag (cadr args))))
          )
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
