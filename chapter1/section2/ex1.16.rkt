#lang racket/base
(require racket/trace)

(define 
  (even? n)
  (= (remainder n 2) 0)
)

; 不変量：a * b^n
; nを半分にする場合：a * b^n = a * (b^2)^{n/2}
; nを-1する場合：a * b^n = a * b * b^{n - 1}
(define 
  (iter a b n)
  (if (= n 0) 
    a
    (if (even? n) 
      (iter a (* b b) (/ n 2))
      (iter (* a b) b (- n 1))
    )
  )
)

(define 
  (fast-expt b n)
  (iter 1 b n)
)

(trace iter)
(fast-expt 2 10)
