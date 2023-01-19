; 再帰的なf
(define 
  (recursive-f n)
  (if (< n 3) 
    n
    (+ 
      (recursive-f (- n 1))
      (* 2 (recursive-f (- n 2)))
      (* 3 (recursive-f (- n 3)))
    )
  )
)

(print (recursive-f 5))
(print (recursive-f 10))
(print (recursive-f 20))
(print (recursive-f 30))

; 手続き的なf
(define 
  (liner-f n)

  (define 
    ; a: f(cnt - 1)
    ; b: f(cnt - 2)
    ; c: f(cnt - 3)
    (iter cnt a b c)
    (if (> cnt n) 
      a
      (iter 
        (+ cnt 1)
        (+ a (* 2 b) (* 3 c))
        a
        b
      )
    )
  )

  (if (< n 3) 
    n
    (iter 3 2 1 0)
  )
)

(print (liner-f 5))
(print (liner-f 10))
(print (liner-f 20))
(print (liner-f 30))
