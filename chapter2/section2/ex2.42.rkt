#lang racket/base

(define 
  (accumulate op initial sequence)
  (if (null? sequence) 
    initial
    (op 
      (car sequence)
      (accumulate op initial (cdr sequence))
    )
  )
)

(define 
  (flatmap proc seq)
  (accumulate append null (map proc seq))
)

(define 
  (enumerate-interval low high)
  (if (> low high) 
    null
    (cons low (enumerate-interval (+ low 1) high))
  )
)

(define 
  (queens board-size)

  ; クイーンの位置は(x . y)という座標で表す。盤面の左上を(0 . 0)とする
  (define (make-position x y) (cons x y))
  (define (position-x p) (car p))
  (define (position-y p) (cdr p))

  ; 盤面はクイーンの位置の集合で表す
  (define empty-board (list))

  ; boardsの(col . row)にクイーンを置く
  (define 
    (adjoin-position row col board)
    (cons (make-position col row) board)
  )

  ; boardのcol列にあるクイーンが、他のクイーンの利きに無いとき#t。そうでないとき#f
  (define 
    (safe? col board)
    (let 
      (
       ; target: 検査対象のクイーン
       (target (car board))
       ; others: 検査対象でないクイーン
       (others (cdr board))
      )

      (accumulate 
        (lambda (x y) (and x y))
        #t
        (map 
          (lambda (other-q) 
            (and 
              ; 横の利き: targetとy座標が同じでない
              (not 
                (= 
                  (position-y other-q)
                  (position-y target)
                )
              )
              ; 斜めの利き：targetとの傾きの絶対値が1でない
              (not 
                (= 
                  (abs 
                    (/ 
                      (- (position-y target) (position-y other-q))
                      (- (position-x target) (position-x other-q))
                    )
                  )
                  1
                )
              )
            )
          )
          others
        )
      )
    )
  )

  (define 
    (queen-cols k)
    (if (= k 0) 
      (list empty-board)
      (filter 
        (lambda (positions) (safe? k positions))
        (flatmap 
          (lambda (rest-of-queens) 
            (map 
              (lambda (new-row) 
                (adjoin-position 
                  new-row
                  k
                  rest-of-queens
                )
              )
              (enumerate-interval 1 board-size)
            )
          )
          (queen-cols (- k 1))
        )
      )
    )
  )

  (queen-cols board-size)
)

(define 
  (print-board board n)
  (for-each 
    (lambda (y) 
      (for-each 
        (lambda (x) 
          (let 
            ((q 
               (findf 
                 (lambda (q) 
                   (and 
                     (= (car q) x)
                     (= (cdr q) y)
                   )
                 )
                 board
               )
             ) 
            )
            (if (pair? q) (display "q") (display "."))
          )
        )
        (enumerate-interval 1 n)
      )
      (newline)
    )
    (enumerate-interval 1 n)
  )
)

(length (queens 8))
; 92

(print-board 
  (car (queens 8))
  8
)
; q.......
; ......q.
; ....q...
; .......q
; .q......
; ...q....
; .....q..
; ..q.....

