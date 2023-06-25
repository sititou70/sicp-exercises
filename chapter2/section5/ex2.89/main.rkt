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
    (list 
      (make-scheme-number 1)
      (make-rational 2 3)
      (make-complex-from-real-imag 1 2)
      (make-complex-from-mag-ang 1 2)
      (make-polynomial 'y (list (make-scheme-number 1)))
    )
  )
  (make-polynomial 
    'x
    (list 
      (make-complex-from-mag-ang 1 2)
      (make-complex-from-real-imag 1 2)
      (make-rational 2 3)
      (make-scheme-number 1)
      (make-polynomial 'y (list (make-scheme-number 1)))
    )
  )
)
; '(polynomial 
;   x
;   (complex rectangular 0.5838531634528576 . 0.9092974268256817)
;   (complex rectangular 5/3 . 2)
;   (complex rectangular 5/3 . 2)
;   (complex rectangular 0.5838531634528576 . 0.9092974268256817)
;   (polynomial y (scheme-number . 2))
; )

(mul 
  (make-polynomial 
    'x
    (list 
      (make-scheme-number 1)
      (make-rational 2 3)
      (make-complex-from-real-imag 1 2)
      (make-complex-from-mag-ang 1 2)
    )
  )
  (make-polynomial 
    'x
    (list 
      (make-complex-from-mag-ang 1 2)
      (make-complex-from-real-imag 1 2)
      (make-rational 2 3)
      (make-scheme-number 1)
    )
  )
)
; '(polynomial 
;   x
;   (complex polar 1 . 2)
;   (complex rectangular 0.7225687756352386 . 2.6061982845504543)
;   (complex rectangular -0.9014083568651724 . 1.4103370870647305)
;   (complex rectangular -2.209199176419167 . 3.2431975046920734)
;   (complex rectangular -0.9014083568651724 . 1.4103370870647305)
;   (complex rectangular 0.7225687756352386 . 2.6061982845504543)
;   (complex rectangular -0.4161468365471424 . 0.9092974268256817)
; )

(mul 
  (make-polynomial 
    'x
    (list 
      (make-scheme-number 1)
      (make-scheme-number 1)
    )
  )
  (make-polynomial 
    'x
    (list 
      (make-scheme-number 1)
      (make-scheme-number -1)
    )
  )
)
; '(polynomial 
;   x
;   (scheme-number . 1)
;   (scheme-number . 0)
;   (scheme-number . -1)
; )
