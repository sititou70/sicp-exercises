#lang racket/base

(require "generic-procs.rkt")
(require "scheme-number.rkt")
(require "rational.rkt")
(require "complex.rkt")
(require "polynomial.rkt")
(require "coercion.rkt")

(add 
  (make-polynomial 
    'x
    (make-polynomial-dense-terms 
      (make-scheme-number 1)
      (make-rational 2 3)
      (make-complex-from-real-imag 1 2)
      (make-complex-from-mag-ang 1 2)
      (make-polynomial 
        'y
        (make-polynomial-sparse-terms 
          (list 0 (make-scheme-number 1))
        )
      )
    )
  )
  (make-polynomial 
    'x
    (make-polynomial-sparse-terms 
      (list 4 (make-complex-from-mag-ang 1 2))
      (list 3 (make-complex-from-real-imag 1 2))
      (list 2 (make-rational 2 3))
      (list 1 (make-scheme-number 1))
      (list 
        0
        (make-polynomial 
          'y
          (make-polynomial-dense-terms 
            (make-scheme-number 1)
          )
        )
      )
    )
  )
)
; '(polynomial 
;   x
;   sparse-terms
;   (4 (complex rectangular 0.5838531634528576 . 0.9092974268256817))
;   (3 (complex rectangular 5/3 . 2))
;   (2 (complex rectangular 5/3 . 2))
;   (1 (complex rectangular 0.5838531634528576 . 0.9092974268256817))
;   (0 (polynomial y dense-terms (scheme-number . 2)))
; )

(mul 
  (make-polynomial 
    'x
    (make-polynomial-dense-terms 
      (make-scheme-number 1)
      (make-rational 2 3)
      (make-complex-from-real-imag 1 2)
      (make-complex-from-mag-ang 1 2)
    )
  )
  (make-polynomial 
    'x
    (make-polynomial-sparse-terms 
      (list 3 (make-complex-from-mag-ang 1 2))
      (list 2 (make-complex-from-real-imag 1 2))
      (list 1 (make-rational 2 3))
      (list 0 (make-scheme-number 1))
    )
  )
)
; '(polynomial 
;   x
;   sparse-terms
;   (6 (complex polar 1 . 2))
;   (5 (complex rectangular 0.7225687756352386 . 2.6061982845504543))
;   (4 (complex rectangular -0.9014083568651724 . 1.4103370870647305))
;   (3 (complex rectangular -2.209199176419167 . 3.2431975046920734))
;   (2 (complex rectangular -0.9014083568651724 . 1.4103370870647305))
;   (1 (complex rectangular 0.7225687756352386 . 2.6061982845504543))
;   (0 (complex polar 1 . 2))
; )

(mul 
  (make-polynomial 
    'x
    (make-polynomial-sparse-terms 
      (list 1 (make-scheme-number 1))
      (list 0 (make-scheme-number 1))
    )
  )
  (make-polynomial 
    'x
    (make-polynomial-dense-terms 
      (make-scheme-number 1)
      (make-scheme-number -1)
    )
  )
)
; '(polynomial 
;   x
;   dense-terms
;   (scheme-number . 1)
;   (scheme-number . 0)
;   (scheme-number . -1)
; )
