(print 10)
; 10

(print (+ 5 3 4))
; 12

(print (- 9 1))
; 8

(print (/ 6 2))
; 3

(print (+ (* 2 4) (- 4 6)))
; 6

(define a 3)
(define b (+ a 1))

(print (+ a b (* a b)))
; 19

(print (= a b))
; #f

(print 
  (if (and (> b a) (< b (* a b))) 
    b
    a
  )
)
; 4

(print 
  (cond 
    ((= a 4) 6)
    ((= b 4) (+ 6 7 a))
    (else 25)
  )
)
; 16

(print (+ 2 (if (> b a) b a)))
; 6

(print 
  (* 
    (cond 
      ((> a b) a)
      ((< a b) b)
      (else -1)
    )
    (+ a 1)
  )
)
; 16
