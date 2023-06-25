#lang racket/base
(require racket/trace)

; 本文に示されている再帰的なsum
(define 
  (sum-rec term a next b)
  (if (> a b) 
    0
    (+ (term a) 
       (sum-rec term (next a) next b)
    )
  )
)
(trace sum-rec)

; この演習で作成する反復的なsum
(define 
  (sum term a next b)
  (define 
    (iter a result)
    (if (> a b) 
      result
      (iter (next a) (+ result (term a)))
    )
  )
  (trace iter)

  (iter a 0)
)

; 以下に2つのsumのトレースを示す。前者は再帰的であり、後者は反復的である
(define (cube x) (* x x x))
(define (inc n) (+ n 1))

(sum-rec cube 1 inc 10)
; >(sum-rec #<procedure:cube> 1 #<procedure:inc> 10)
; > (sum-rec #<procedure:cube> 2 #<procedure:inc> 10)
; > >(sum-rec #<procedure:cube> 3 #<procedure:inc> 10)
; > > (sum-rec #<procedure:cube> 4 #<procedure:inc> 10)
; > > >(sum-rec #<procedure:cube> 5 #<procedure:inc> 10)
; > > > (sum-rec #<procedure:cube> 6 #<procedure:inc> 10)
; > > > >(sum-rec #<procedure:cube> 7 #<procedure:inc> 10)
; > > > > (sum-rec #<procedure:cube> 8 #<procedure:inc> 10)
; > > > > >(sum-rec #<procedure:cube> 9 #<procedure:inc> 10)
; > > > > > (sum-rec #<procedure:cube> 10 #<procedure:inc> 10)
; > > > >[10] (sum-rec #<procedure:cube> 11 #<procedure:inc> 10)
; < < < <[10] 0
; < < < < < 1000
; < < < < <1729
; < < < < 2241
; < < < <2584
; < < < 2800
; < < <2925
; < < 2989
; < <3016
; < 3024
; <3025
; 3025

(sum cube 1 inc 10)
; >(iter 1 0)
; >(iter 2 1)
; >(iter 3 9)
; >(iter 4 36)
; >(iter 5 100)
; >(iter 6 225)
; >(iter 7 441)
; >(iter 8 784)
; >(iter 9 1296)
; >(iter 10 2025)
; >(iter 11 3025)
; <3025
; 3025
