#lang racket
(provide (all-defined-out))

(require sicp)
(require "q-eval/repl.scm")

(define 
  (insert-sample-data)

  (define repl (make-repl 'silent))

  (repl '(assert! (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))))
  (repl '(assert! (job (Bitdiddle Ben) (computer wizard))))
  (repl '(assert! (salary (Bitdiddle Ben) 60000)))

  (repl '(assert! (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))))
  (repl '(assert! (job (Hacker Alyssa P) (computer programmer))))
  (repl '(assert! (salary (Hacker Alyssa P) 40000)))
  (repl '(assert! (supervisor (Hacker Alyssa P) (Bitdiddle Ben))))

  (repl '(assert! (address (Fect Cy D) (Cambridge (Ames Street) 3))))
  (repl '(assert! (job (Fect Cy D) (computer programmer))))
  (repl '(assert! (salary (Fect Cy D) 35000)))
  (repl '(assert! (supervisor (Fect Cy D) (Bitdiddle Ben))))

  (repl '(assert! (address (Tweakit Lem E) (Boston (Bay State Road) 22))))
  (repl '(assert! (job (Tweakit Lem E) (computer technician))))
  (repl '(assert! (salary (Tweakit Lem E) 25000)))
  (repl '(assert! (supervisor (Tweakit Lem E) (Bitdiddle Ben))))

  (repl '(assert! (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))))
  (repl '(assert! (job (Reasoner Louis) (computer programmer trainee))))
  (repl '(assert! (salary (Reasoner Louis) 30000)))
  (repl '(assert! (supervisor (Reasoner Louis) (Hacker Alyssa P))))
  (repl '(assert! (supervisor (Bitdiddle Ben) (Warbucks Oliver))))

  (repl '(assert! (address (Warbucks Oliver) (Swellesley (Top Heap Road)))))
  (repl '(assert! (job (Warbucks Oliver) (administration big wheel))))
  (repl '(assert! (salary (Warbucks Oliver) 150000)))

  (repl '(assert! (address (Scrooge Eben) (Weston (Shady Lane) 10))))
  (repl '(assert! (job (Scrooge Eben) (accounting chief accountant))))
  (repl '(assert! (salary (Scrooge Eben) 75000)))
  (repl '(assert! (supervisor (Scrooge Eben) (Warbucks Oliver))))

  (repl '(assert! (address (Cratchet Robert) (Allston (N Harvard Street) 16))))
  (repl '(assert! (job (Cratchet Robert) (accounting scrivener))))
  (repl '(assert! (salary (Cratchet Robert) 18000)))
  (repl '(assert! (supervisor (Cratchet Robert) (Scrooge Eben))))

  (repl '(assert! (address (Aull DeWitt) (Slumerville (Onion Square) 5))))
  (repl '(assert! (job (Aull DeWitt) (administration secretary))))
  (repl '(assert! (salary (Aull DeWitt) 25000)))
  (repl '(assert! (supervisor (Aull DeWitt) (Warbucks Oliver))))

  (repl '(assert! (can-do-job (computer wizard) (computer programmer))))
  (repl '(assert! (can-do-job (computer wizard) (computer technician))))
  (repl 
    '(assert! (can-do-job (computer programmer) (computer programmer trainee)))
  )
  (repl 
    '(assert! (can-do-job (administration secretary) (administration big wheel)))
  )
)
