#lang racket
(provide (all-defined-out))

(require sicp)
(require "q-eval/repl.rkt")
(require "sample-db.rkt")

(define repl (make-repl 'display))
(insert-sample-data)

; main
(repl 
  '
  (assert! 
    (rule 
      (same ?x ?x)
    )
  )
)

(repl 
  '
  (assert! 
    (rule 
      (son ?m ?s)
      (and 
        (wife ?m ?w)
        (son ?w ?s)
      )
    )
  )
)

(repl 
  '
  (assert! 
    (rule 
      ((grandson) ?g ?s)
      (and 
        (son ?g ?f)
        (son ?f ?s)
      )
    )
  )
)

(repl 
  '
  (assert! 
    (rule 
      (end-with (?first . ?rest) ?end)
      (or 
        (and 
          (same ?rest ())
          (same ?first ?end)
        )
        (end-with ?rest ?end)
      )
    )
  )
)

(repl 
  '
  (assert! 
    (rule 
      ((great . ?rel) ?x ?y)
      (and 
        (son ?x ?s)
        (?rel ?s ?y)
        (end-with ?rel grandson)
      )
    )
  )
)

(repl '((great grandson) ?g ?ggs))
; {{great grandson} Mehujael Jubal}
; {{great grandson} Irad Lamech}
; {{great grandson} Mehujael Jabal}
; {{great grandson} Enoch Methushael}
; {{great grandson} Cain Mehujael}
; {{great grandson} Adam Irad}

(repl '((great great great great great grandson) ?g ?ggs))
; {{great great great great great grandson} Adam Jubal}
; {{great great great great great grandson} Adam Jabal}

(repl '(?relationship Adam Irad))
; {{great grandson} Adam Irad}
