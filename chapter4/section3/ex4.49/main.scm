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
  (define 
    (parse-word word-list)

    ; とりあえずリストの最初の単語を選択する
    (define selected-word (car (cdr word-list)))

    (list (car word-list) selected-word)
    (set! *unparsed* (append *unparsed* (list selected-word)))
  )
)

(repl 
  '
  (define 
    (parse-prepositional-phrase)
    (list 'prep-phrase 
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
          (list 'verb-phrase 
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
    (parse-sentence)
    (list 'sentence (parse-noun-phrase) (parse-verb-phrase))
  )
)

(repl 
  '
  (define *unparsed* '())
)
(repl 
  '
  (define 
    (generate)
    (parse-sentence)
    *unparsed*
  )
)

(repl 
  '(generate)
)
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
; {the student studies}
; {the student studies for the student}
; {the student studies for the student for the student}
; {the student studies for the student for the student for the student}
; {the student studies for the student for the student for the student for the student}

; 文法が再帰的であるため、前置詞句が単調に再帰する文章が生成されており、あまりおもしろくない。
