#lang racket/base
(require racket/trace)

(define 
  (product-rec term a next b)
  (if (> a b) 
    1
    (* (term a) 
       (product-rec term (next a) next b)
    )
  )
)
(trace product-rec)

(define 
  (product-iter term a next b)
  (define 
    (iter a result)
    (if (> a b) 
      result
      (iter (next a) (* result (term a)))
    )
  )
  (trace iter)

  (iter a 1)
)

; 再帰的および線形的なproductによって5の階乗を求める
(define (identity n) n)
(define (inc n) (+ n 1))
(product-rec identity 1 inc 5)
; >(product-rec #<procedure:identity> 1 #<procedure:inc> 5)
; > (product-rec #<procedure:identity> 2 #<procedure:inc> 5)
; > >(product-rec #<procedure:identity> 3 #<procedure:inc> 5)
; > > (product-rec #<procedure:identity> 4 #<procedure:inc> 5)
; > > >(product-rec #<procedure:identity> 5 #<procedure:inc> 5)
; > > > (product-rec #<procedure:identity> 6 #<procedure:inc> 5)
; < < < 1
; < < <5
; < < 20
; < <60
; < 120
; <120
; 120
(product-iter identity 1 inc 5)
; >(iter 1 1)
; >(iter 2 1)
; >(iter 3 2)
; >(iter 4 6)
; >(iter 5 24)
; >(iter 6 120)
; <120
; 120

; 線形的および再帰的productによって円周率の近似値を求める
(define 
  (pi-term n)
  (/ 
    (* (+ (floor (/ n 2.0)) 1) 2)
    (+ (* (ceiling (/ n 2.0)) 2) 1)
  )
)
(* (product-rec pi-term 1 inc 10) 4)
; 3.275101041334807
(* (product-iter pi-term 1 inc 10) 4)
; 3.2751010413348065
