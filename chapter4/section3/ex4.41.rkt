#lang racket

(define 
  (flatmap op list)
  (apply 
    append
    (map 
      op
      list
    )
  )
)

(define 
  (distinct? items)
  (cond 
    ((null? items) true)
    ((null? (cdr items)) true)
    ((member (car items) (cdr items)) false)
    (else (distinct? (cdr items)))
  )
)

(define 
  (multiple-dwelling)
  (flatmap 
    (lambda (baker) 
      (flatmap 
        (lambda (cooper) 
          (flatmap 
            (lambda (fletcher) 
              (flatmap 
                (lambda (miller) 
                  (flatmap 
                    (lambda (smith) 
                      (if 
                        (and 
                          (distinct? (list baker cooper fletcher miller smith))
                          (not (= baker 5))
                          (not (= cooper 1))
                          (not (= fletcher 5))
                          (not (= fletcher 1))
                          (> miller cooper)
                          (not (= (abs (- smith fletcher)) 1))
                          (not (= (abs (- fletcher cooper)) 1))
                        )

                        (list 
                          (list 'baker baker)
                          (list 'cooper cooper)
                          (list 'fletcher fletcher)
                          (list 'miller miller)
                          (list 'smith smith)
                        )

                        '()
                      )
                    )
                    '(1 2 3 4 5)
                  )
                )
                '(1 2 3 4 5)
              )
            )
            '(1 2 3 4 5)
          )
        )
        '(1 2 3 4 5)
      )
    )
    '(1 2 3 4 5)
  )
)

(multiple-dwelling)
