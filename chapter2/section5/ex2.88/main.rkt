#lang racket/base

(require "generic-procs.rkt")
(require "scheme-number.rkt")
(require "rational.rkt")
(require "complex.rkt")
(require "polynomial.rkt")
(require "coercion.rkt")

(additive-inverse (make-scheme-number 1))
; '(scheme-number . -1)
(additive-inverse (make-rational 1 2))
; '(scheme-number . -1/2)
(additive-inverse (make-complex-from-real-imag 1 2))
; '(complex polar -2.23606797749979 . 1.1071487177940904)
(additive-inverse (make-complex-from-mag-ang 1 2))
; '(complex polar -1 . 2)
(additive-inverse 
  (make-polynomial 
    'x
    (list 
      (list 0 (make-scheme-number 1))
    )
  )
)
; '(polynomial x (0 (scheme-number . -1)))

(sub 
  (make-polynomial 
    'x
    (list 
      (list 2 (make-scheme-number 3))
      (list 1 (make-complex-from-real-imag 2 3))
      (list 0 (make-scheme-number 7))
    )
  )
  (make-polynomial 
    'x
    (list 
      (list 4 (make-scheme-number 1))
      (list 2 (make-rational 2 3))
      (list 0 (make-complex-from-real-imag 5 3))
    )
  )
)
; '(polynomial 
;   x
;   (4 (scheme-number . -1))
;   (2 (scheme-number . 7/3))
;   (1 (complex rectangular 2 . 3))
;   (0 (complex rectangular 2.0 . -3.0000000000000004))
; )

(sub 
  (make-polynomial 
    'x
    (list 
      (list 
        2
        (make-polynomial 
          'y
          (list 
            (list 1 (make-scheme-number 1))
            (list 0 (make-scheme-number 1))
          )
        )
      )
      (list 
        1
        (make-polynomial 
          'y
          (list 
            (list 2 (make-scheme-number 1))
            (list 0 (make-scheme-number 1))
          )
        )
      )
      (list 
        0
        (make-polynomial 
          'y
          (list 
            (list 1 (make-scheme-number 1))
            (list 0 (make-scheme-number -1))
          )
        )
      )
    )
  )
  (make-polynomial 
    'x
    (list 
      (list 
        1
        (make-polynomial 
          'y
          (list 
            (list 1 (make-scheme-number 1))
            (list 0 (make-scheme-number -2))
          )
        )
      )
      (list 
        0
        (make-polynomial 
          'y
          (list 
            (list 3 (make-scheme-number 1))
            (list 0 (make-scheme-number 7))
          )
        )
      )
    )
  )
)
; '(polynomial 
;   x
;   (2 (polynomial y (1 (scheme-number . 1)) (0 (scheme-number . 1))))
;   (1 
;     (polynomial 
;       y
;       (2 (scheme-number . 1))
;       (1 (scheme-number . -1))
;       (0 (scheme-number . 3))
;     )
;   )
;   (0 
;     (polynomial 
;       y
;       (3 (scheme-number . -1))
;       (1 (scheme-number . 1))
;       (0 (scheme-number . -8))
;     )
;   )
; )
