#lang racket/base

(define 
  (make-from-mag-ang mag ang)

  (define 
    (dispatch op)
    (cond 
      ((eq? op 'real-part) (* mag (cos ang)))
      ((eq? op 'imag-part) (* mag (sin ang)))
      ((eq? op 'magnitude) mag)
      ((eq? op 'angle) ang)
      (else (error " Unknown op: MAKE-FROM-MAG-ANG " op))
    )
  )

  dispatch
)

(define z (make-from-mag-ang 2 (/ 3.1415926535 6)))

(z 'real-part)
; 1.7320508075838428
(z 'imag-part)
; 0.999999999974079
(z 'magnitude)
; 2
(z 'angle)
; 0.5235987755833333
(z 'hoge)
; Unknown op: MAKE-FROM-MAG-ANG  'hoge
