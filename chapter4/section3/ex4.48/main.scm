#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "m-eval/eval-apply.scm")
(require "m-eval/global-environment.scm")

(define 
  (make-repl)

  (define current-problem '*no-input*)
  (define current-failure-continuation '())

  (define display-limit 70)
  (define 
    (displaylimitln item)

    (define o (open-output-string))
    (write item o)
    (define str (get-output-string o))

    (if (< display-limit (string-length str)) 
      (begin 
        (display (substring str 0 display-limit))
        (displayln "...")
      )
      (displayln str)
    )
  )

  (define 
    (repl input)

    (if (eq? input 'try-again) 
      (begin 
        (if (null? current-failure-continuation) 
          (displayln ";;; There is no current problem")
          (current-failure-continuation)
        )
      )
      (begin 
        (display ";;; Starting a new problem: ")
        (displaylimitln input)

        (set! current-problem input)
        (ambeval 
          input
          the-global-environment
          (lambda (val next-alternative) 
            (displayln val)
            (set! current-failure-continuation next-alternative)
          )
          (lambda () 
            (display 
              ";;; There are no more values of: "
            )
            (displaylimitln current-problem)
          )
        )
      )
    )
  )

  repl
)
(define repl (make-repl))

; main
(repl 
  '
  (define 
    (require p)
    (if (not p) (amb))
  )
)

(repl 
  '
  (define nouns '(noun student professor cat class))
)
(repl 
  '
  (define verbs '(verb studies lectures eats sleeps))
)
(repl 
  '
  (define articles '(article the a))
)
(repl 
  '
  (define prepositions '(prep for to in by with))
)
(repl 
  '
  (define compounds '(compound and or but))
)

(repl 
  '
  (define 
    (parse-word word-list)
    (require (not (null? *unparsed*)))
    (require (memq (car *unparsed*) (cdr word-list)))
    (let 
      ((found-word (car *unparsed*)))
      (set! *unparsed* (cdr *unparsed*))
      (list (car word-list) found-word)
    )
  )
)

(repl 
  '
  (define 
    (parse-prepositional-phrase)
    (list 
      'prep-phrase
      (parse-word prepositions)
      (parse-noun-phrase)
    )
  )
)

(repl 
  '
  (define 
    (parse-verb-phrase)
    (define 
      (maybe-extend verb-phrase)
      (amb 
        verb-phrase
        (maybe-extend 
          (list 
            'verb-phrase
            verb-phrase
            (parse-prepositional-phrase)
          )
        )
      )
    )
    (maybe-extend (parse-word verbs))
  )
)

(repl 
  '
  (define 
    (parse-simple-noun-phrase)
    (list 'simple-noun-phrase 
          (parse-word articles)
          (parse-word nouns)
    )
  )
)
(repl 
  '
  (define 
    (parse-noun-phrase)
    (define 
      (maybe-extend noun-phrase)
      (amb 
        noun-phrase
        (maybe-extend 
          (list 'noun-phrase 
                noun-phrase
                (parse-prepositional-phrase)
          )
        )
      )
    )
    (maybe-extend (parse-simple-noun-phrase))
  )
)

(repl 
  '
  (define 
    (parse-simple-sentence)
    (list 
      'simple-sentence
      (parse-noun-phrase)
      (parse-verb-phrase)
    )
  )
)


(repl 
  '
  (define 
    (parse-sentence)
    (define 
      (maybe-extend sentence)
      (amb 
        sentence
        (maybe-extend 
          (list 
            'compound-sentence
            sentence
            (parse-word compounds)
            (parse-sentence)
          )
        )
      )
    )
    (maybe-extend (parse-simple-sentence))
  )
)

(repl 
  '
  (define *unparsed* '())
)
(repl 
  '
  (define 
    (parse input)
    (set! *unparsed* input)
    (let 
      ((sent (parse-sentence)))
      (require (null? *unparsed*))
      sent
    )
  )
)

(repl 
  '(parse
    '(the professor lectures to the student but the student sleeps in the class)
   )
)
(repl 'try-again)
; {compound-sentence {simple-sentence {simple-noun-phrase {article the} {noun professor}} {verb-phrase {verb lectures} {prep-phrase {prep to} {simple-noun-phrase {article the} {noun student}}}}} {compound but} {simple-sentence {simple-noun-phrase {article the} {noun student}} {verb-phrase {verb sleeps} {prep-phrase {prep in} {simple-noun-phrase {article the} {noun class}}}}}}
; (compound-sentence 
;   (simple-sentence 
;     (simple-noun-phrase (article the) (noun professor))
;     (verb-phrase 
;       (verb lectures)
;       (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student)))
;     )
;   )
;   (compound but)
;   (simple-sentence 
;     (simple-noun-phrase (article the) (noun student))
;     (verb-phrase 
;       (verb sleeps)
;       (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class)))
;     )
;   )
; )
;;; There are no more values of: (parse (quote (the professor lectures to the student but the student s...
