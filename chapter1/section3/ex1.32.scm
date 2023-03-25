#lang racket/base
(require racket/trace)

(define 
  (accumulate-rec combiner null-value term a next b)

  (if (> a b) 
    null-value
    (combiner 
      (term a)
      (accumulate-rec combiner null-value term (next a) next b)
    )
  )
)
(trace accumulate-rec)

(define 
  (accumulate-iter combiner null-value term a next b)

  (define 
    (iter a result)
    (if (> a b) 
      result
      (iter (next a) (combiner result (term a)))
    )
  )
  (trace iter)

  (iter a null-value)
)
(trace accumulate-rec)

(define (identity n) n)
(define (inc n) (+ n 1))

; 再帰的および反復的なaccumlateで1から5までの総和と5の階乗を求める
(accumulate-rec + 0 identity 1 inc 5)
; >(accumulate-rec #<procedure:+> 0 #<procedure:identity> 1 #<procedure:inc> 5)
; > (accumulate-rec #<procedure:+> 0 #<procedure:identity> 2 #<procedure:inc> 5)
; > >(accumulate-rec #<procedure:+> 0 #<procedure:identity> 3 #<procedure:inc> 5)
; > > (accumulate-rec
;      #<procedure:+>
;      0
;      #<procedure:identity>
;      4
;      #<procedure:inc>
;      5)
; > > >(accumulate-rec
;       #<procedure:+>
;       0
;       #<procedure:identity>
;       5
;       #<procedure:inc>
;       5)
; > > > (accumulate-rec
;        #<procedure:+>
;        0
;        #<procedure:identity>
;        6
;        #<procedure:inc>
;        5)
; < < < 0
; < < <5
; < < 9
; < <12
; < 14
; <15
; 15
(accumulate-rec * 1 identity 1 inc 5)
; >(accumulate-rec #<procedure:*> 1 #<procedure:identity> 1 #<procedure:inc> 5)
; > (accumulate-rec #<procedure:*> 1 #<procedure:identity> 2 #<procedure:inc> 5)
; > >(accumulate-rec #<procedure:*> 1 #<procedure:identity> 3 #<procedure:inc> 5)
; > > (accumulate-rec
;      #<procedure:*>
;      1
;      #<procedure:identity>
;      4
;      #<procedure:inc>
;      5)
; > > >(accumulate-rec
;       #<procedure:*>
;       1
;       #<procedure:identity>
;       5
;       #<procedure:inc>
;       5)
; > > > (accumulate-rec
;        #<procedure:*>
;        1
;        #<procedure:identity>
;        6
;        #<procedure:inc>
;        5)
; < < < 1
; < < <5
; < < 20
; < <60
; < 120
; <120
; 120
(accumulate-iter + 0 identity 1 inc 5)
; >(iter 1 0)
; >(iter 2 1)
; >(iter 3 3)
; >(iter 4 6)
; >(iter 5 10)
; >(iter 6 15)
; <15
; 15
(accumulate-iter * 1 identity 1 inc 5)
; >(iter 1 1)
; >(iter 2 1)
; >(iter 3 2)
; >(iter 4 6)
; >(iter 5 24)
; >(iter 6 120)
; <120
; 120
