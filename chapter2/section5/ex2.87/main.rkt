#lang racket/base

(require "generic-procs.rkt")
(require "scheme-number.rkt")
(require "rational.rkt")
(require "complex.rkt")
(require "polynomial.rkt")
(require "coercion.rkt")

(mul 
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
; '
; (polynomial 
;   x
;   (6 (scheme-number . 3))
;   (5 (complex polar 3.605551275463989 . 0.982793723247329))
;   (4 (scheme-number . 9))
;   (3 (complex polar 2.403700850309326 . 0.982793723247329))
;   (2 (complex rectangular 19.666666666666668 . 9.000000000000002))
;   (1 (complex polar 21.02379604162864 . 1.5232132235179132))
;   (0 (complex polar 40.81666326391711 . 0.5404195002705842))
; )

(mul 
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
; '
; (polynomial 
;   x
;   (3 
;     (polynomial 
;       y
;       (2 (scheme-number . 1))
;       (1 (scheme-number . -1))
;       (0 (scheme-number . -2))
;     )
;   )
;   (2 
;     (polynomial 
;       y
;       (4 (scheme-number . 1))
;       (3 (scheme-number . 2))
;       (2 (scheme-number . -2))
;       (1 (scheme-number . 8))
;       (0 (scheme-number . 5))
;     )
;   )
;   (1 
;     (polynomial 
;       y
;       (5 (scheme-number . 1))
;       (3 (scheme-number . 1))
;       (2 (scheme-number . 8))
;       (1 (scheme-number . -3))
;       (0 (scheme-number . 9))
;     )
;   )
;   (0 
;     (polynomial 
;       y
;       (4 (scheme-number . 1))
;       (3 (scheme-number . -1))
;       (1 (scheme-number . 7))
;       (0 (scheme-number . -7))
;     )
;   )
; )
