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
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define (weight i j) (+ (* i i i) (* j j j)))
(define 
  pairs
  (weighted-pairs 
    integers
    integers
    weight
  )
)
(define 
  (ramanujan-numbers)

  (define 
    (iter pairs)
    (let 
      ( ;
       (pair1 (stream-car pairs))
       (pair2 (stream-car (stream-cdr pairs)))
       (pair3 (stream-car (stream-cdr (stream-cdr pairs))))
       (pair4 (stream-car (stream-cdr (stream-cdr (stream-cdr pairs)))))
      )

      (if 
        (and 
          (not (= (apply weight pair1) (apply weight pair2)))
          (= (apply weight pair2) (apply weight pair3))
          (not (= (apply weight pair3) (apply weight pair4)))
        )
        (cons-stream 
          (list (apply weight pair2) pair2 pair3)
          (iter (stream-cdr pairs))
        )
        (iter (stream-cdr pairs))
      )
    )
  )

  (iter 
    (cons-stream 
      (list -1 -1) ; 番兵
      pairs
    )
  )
)

(print-stream-nth (ramanujan-numbers) 30)
; [0] (1729 (1 12) (9 10))

; 次の5個は以下の通り
; [1] (4104 (2 16) (9 15))
; [2] (13832 (2 24) (18 20))
; [3] (20683 (10 27) (19 24))
; [4] (32832 (4 32) (18 30))
; [5] (39312 (2 34) (15 33))

; その後は以下のように続く
; [6] (40033 (9 34) (16 33))
; [7] (46683 (3 36) (27 30))
; [8] (64232 (17 39) (26 36))
; [9] (65728 (12 40) (31 33))
; [10] (110656 (4 48) (36 40))
; [11] (110808 (6 48) (27 45))
; [12] (134379 (12 51) (38 43))
; [13] (149389 (8 53) (29 50))
; [14] (165464 (20 54) (38 48))
; [15] (171288 (17 55) (24 54))
; [16] (195841 (9 58) (22 57))
; [17] (216027 (3 60) (22 59))
; [18] (216125 (5 60) (45 50))
; [19] (262656 (8 64) (36 60))
; [20] (314496 (4 68) (30 66))
; [21] (320264 (18 68) (32 66))
; [22] (327763 (30 67) (51 58))
; [23] (373464 (6 72) (54 60))
; [24] (402597 (42 69) (56 61))
; [25] (439101 (5 76) (48 69))
; [26] (443889 (17 76) (38 73))
; [27] (513000 (10 80) (45 75))
; [28] (513856 (34 78) (52 72))
; [29] (515375 (15 80) (54 71))
