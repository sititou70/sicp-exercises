#lang racket/base

(require "generic-procs.scm")
(require "scheme-number.scm")
(require "rational.scm")
(require "complex.scm")
(require "polynomial.scm")
(require "coercion.scm")

(add 
  (make-polynomial 
    'x
    (list 
      (list 
        1
        (make-polynomial 
          'y
          (list 
            (list 1 (make-scheme-number 2))
            (list 0 (make-scheme-number 1))
          )
        )
      )
    )
  )
  (make-polynomial 
    'y
    (list 
      (list 
        1
        (make-polynomial 
          'x
          (list 
            (list 1 (make-scheme-number 2))
            (list 0 (make-scheme-number 1))
          )
        )
      )
    )
  )
)
; '(polynomial 
;   x
;   (1 (polynomial y (1 (scheme-number . 4)) (0 (scheme-number . 1))))
;   (0 (polynomial y (1 (scheme-number . 1))))
; )
; (2y + 1)x + (2x + 1)y -> (4y + 1)x + y

(add 
  (make-polynomial 
    'x
    (list)
  )
  (make-polynomial 
    'y
    (list 
      (list 
        1
        (make-polynomial 
          'z
          (list 
            (list 
              2
              (make-polynomial 
                'x
                (list 
                  (list 2 (make-scheme-number 1))
                )
              )
            )
            (list 
              1
              (make-polynomial 
                'x
                (list 
                  (list 1 (make-scheme-number 1))
                )
              )
            )
          )
        )
      )
    )
  )
)
; '(polynomial 
;   x
;   (2 (polynomial y (1 (polynomial z (2 (scheme-number . 1))))))
;   (1 (polynomial y (1 (polynomial z (1 (scheme-number . 1))))))
; )
; 0 + ((x^2)z^2 + (x)z)y -> ((z^2)y^2)x^2 + ((y)z)x

(mul 
  (make-polynomial 
    'x
    (list 
      (list 
        1
        (make-polynomial 
          'y
          (list 
            (list 1 (make-scheme-number 2))
            (list 0 (make-scheme-number 1))
          )
        )
      )
    )
  )
  (make-polynomial 
    'y
    (list 
      (list 
        1
        (make-polynomial 
          'x
          (list 
            (list 1 (make-scheme-number 2))
            (list 0 (make-scheme-number 1))
          )
        )
      )
    )
  )
)
; '(polynomial 
;   x
;   (2 (polynomial y (2 (scheme-number . 4)) (1 (scheme-number . 2))))
;   (1 (polynomial y (2 (scheme-number . 2)) (1 (scheme-number . 1))))
; )
; (2y + 1)x * (2x + 1)y -> (4y^2 + 2y)x^2 + (2y^2 + y))x
