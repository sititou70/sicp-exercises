#lang racket

; from: https://stackoverflow.com/questions/13998388/running-code-from-sicp-section-3-5-4-with-drracket
(define the-empty-stream '())
(define 
  (stream-null? stream)
  (null? stream)
)
(define-syntax 
  cons-stream
  (syntax-rules 
    ()
    ((cons-stream head tail) 
      (cons head (delay tail))
    )
  )
)
(define 
  (stream-car stream)
  (car stream)
)
(define 
  (stream-cdr stream)
  (force (cdr stream))
)

; utils
(define 
  (stream-map proc . argstreams)
  (if (stream-null? (car argstreams)) 
    the-empty-stream
    (cons-stream 
      (apply proc (map stream-car argstreams))
      (apply stream-map 
             (cons proc (map stream-cdr argstreams))
      )
    )
  )
)

(define 
  (stream-filter pred stream)
  (cond 
    ((stream-null? stream) the-empty-stream)
    ((pred (stream-car stream))
     (cons-stream 
       (stream-car stream)
       (stream-filter 
         pred
         (stream-cdr stream)
       )
     )
    )
    (else (stream-filter pred (stream-cdr stream)))
  )
)

(define (add-streams s1 s2) (stream-map + s1 s2))

(define 
  (merge-weighted s1 s2 weight)
  (cond 
    ((stream-null? s1) s2)
    ((stream-null? s2) s1)
    (else
     (let 
       ( ;
        (s1car (stream-car s1))
        (s2car (stream-car s2))
        (weight-s1 (apply weight (stream-car s1)))
        (weight-s2 (apply weight (stream-car s2)))
       )
       (cond 
         ((> weight-s1 weight-s2)
          (cons-stream 
            s2car
            (merge-weighted s1 (stream-cdr s2) weight)
          )
         )
         (else
          (cons-stream 
            s1car
            (merge-weighted (stream-cdr s1) s2 weight)
          )
         )
       )
     )
    )
  )
)

(define 
  (weighted-pairs s t weight)
  (cons-stream 
    (list (stream-car s) (stream-car t))
    (merge-weighted 
      (stream-map 
        (lambda (x) (list (stream-car s) x))
        (stream-cdr t)
      )
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
      weight
    )
  )
)

(define 
  (print-stream-nth s n)
  (define 
    (iter s cnt)
    (display "[")
    (display cnt)
    (display "] ")
    (displayln (stream-car s))
    (if (< cnt (- n 1)) 
      (iter (stream-cdr s) (+ cnt 1))
      'done
    )
  )
  (iter s 0)
)

; main
; ラマヌジャン数のweightと、それが連続するか判定する条件を変更するだけで所望のストリームを求められる。
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (weight i j) (+ (* i i) (* j j)))
(define 
  pairs
  (weighted-pairs 
    integers
    integers
    weight
  )
)
(define 
  (s)

  (define 
    (iter pairs)
    (let 
      ( ;
       (pair1 (stream-car pairs))
       (pair2 (stream-car (stream-cdr pairs)))
       (pair3 (stream-car (stream-cdr (stream-cdr pairs))))
       (pair4 (stream-car (stream-cdr (stream-cdr (stream-cdr pairs)))))
       (pair5 
         (stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr pairs)))))
       )
      )

      (if 
        (and 
          (not (= (apply weight pair1) (apply weight pair2)))
          (= (apply weight pair2) (apply weight pair3) (apply weight pair4))
          (not (= (apply weight pair4) (apply weight pair5)))
        )
        (cons-stream 
          (list (apply weight pair2) pair2 pair3 pair4)
          (iter (stream-cdr pairs))
        )
        (iter (stream-cdr pairs))
      )
    )
  )

  (iter 
    (cons-stream 
      (list 1 0.5) ; 番兵
      pairs
    )
  )
)

(print-stream-nth (s) 30)
; [0] (325 (1 18) (6 17) (10 15))
; [1] (425 (5 20) (8 19) (13 16))
; [2] (650 (5 25) (11 23) (17 19))
; [3] (725 (7 26) (10 25) (14 23))
; [4] (845 (2 29) (13 26) (19 22))
; [5] (850 (3 29) (11 27) (15 25))
; [6] (925 (5 30) (14 27) (21 22))
; [7] (1025 (1 32) (8 31) (20 25))
; [8] (1250 (5 35) (17 31) (25 25))
; [9] (1300 (2 36) (12 34) (20 30))
; [10] (1325 (10 35) (13 34) (22 29))
; [11] (1445 (1 38) (17 34) (22 31))
; [12] (1450 (9 37) (15 35) (19 33))
; [13] (1525 (2 39) (9 38) (25 30))
; [14] (1690 (3 41) (13 39) (27 31))
; [15] (1700 (10 40) (16 38) (26 32))
; [16] (1825 (12 41) (15 40) (23 36))
; [17] (1850 (1 43) (13 41) (25 35))
; [18] (2050 (5 45) (23 39) (31 33))
; [19] (2225 (4 47) (17 44) (25 40))
; [20] (2425 (11 48) (20 45) (24 43))
; [21] (2525 (5 50) (26 43) (34 37))
; [22] (2600 (10 50) (22 46) (34 38))
; [23] (2650 (7 51) (21 47) (25 45))
; [24] (2725 (15 50) (18 49) (31 42))
; [25] (2825 (4 53) (11 52) (35 40))
; [26] (2873 (8 53) (13 52) (32 43))
; [27] (2890 (9 53) (17 51) (37 39))
; [28] (2900 (14 52) (20 50) (28 46))
; [29] (2925 (3 54) (18 51) (30 45))
