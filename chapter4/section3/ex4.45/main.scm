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
  '(parse '(the professor lectures to the student in the class with the cat))
)
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
(repl 'try-again)
; {sentence {simple-noun-phrase {article the} {noun professor}} {verb-phrase {verb-phrase {verb-phrase {verb lectures} {prep-phrase {prep to} {simple-noun-phrase {article the} {noun student}}}} {prep-phrase {prep in} {simple-noun-phrase {article the} {noun class}}}} {prep-phrase {prep with} {simple-noun-phrase {article the} {noun cat}}}}}
; {sentence {simple-noun-phrase {article the} {noun professor}} {verb-phrase {verb-phrase {verb lectures} {prep-phrase {prep to} {simple-noun-phrase {article the} {noun student}}}} {prep-phrase {prep in} {noun-phrase {simple-noun-phrase {article the} {noun class}} {prep-phrase {prep with} {simple-noun-phrase {article the} {noun cat}}}}}}}
; {sentence {simple-noun-phrase {article the} {noun professor}} {verb-phrase {verb-phrase {verb lectures} {prep-phrase {prep to} {noun-phrase {simple-noun-phrase {article the} {noun student}} {prep-phrase {prep in} {simple-noun-phrase {article the} {noun class}}}}}} {prep-phrase {prep with} {simple-noun-phrase {article the} {noun cat}}}}}
; {sentence {simple-noun-phrase {article the} {noun professor}} {verb-phrase {verb lectures} {prep-phrase {prep to} {noun-phrase {noun-phrase {simple-noun-phrase {article the} {noun student}} {prep-phrase {prep in} {simple-noun-phrase {article the} {noun class}}}} {prep-phrase {prep with} {simple-noun-phrase {article the} {noun cat}}}}}}}
; {sentence {simple-noun-phrase {article the} {noun professor}} {verb-phrase {verb lectures} {prep-phrase {prep to} {noun-phrase {simple-noun-phrase {article the} {noun student}} {prep-phrase {prep in} {noun-phrase {simple-noun-phrase {article the} {noun class}} {prep-phrase {prep with} {simple-noun-phrase {article the} {noun cat}}}}}}}}}
;;; There are no more values of: (parse (quote (the professor lectures to the student in the class with...

; 以下は、上記の結果をより見やすくしたものである。
; 結果を見るコツ：prep-phraseは、同じ階層の1つ前のverbやnounにかかる。
; 1つ前がphraseであった場合、その中の、深さ優先順序で一番最初に見つかるverbやnounにかかる

; (sentence 
;   (simple-noun-phrase (article the) (noun professor))
;   (verb-phrase 
;     (verb-phrase 
;       (verb-phrase 
;         (verb lectures)
;         (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student)))
;       )
;       (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class)))
;     )
;     (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))
;   )
; )
; 教授は、講義する
;         ----
;           |__to 学生
;           |__in 教室
;           |__with 猫
; 教授は学生に講義する。講義は教室で行う。講義は猫と協力して行う。

; (sentence 
;   (simple-noun-phrase (article the) (noun professor))
;   (verb-phrase 
;     (verb-phrase 
;       (verb lectures)
;       (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student)))
;     )
;     (prep-phrase 
;       (prep in)
;       (noun-phrase 
;         (simple-noun-phrase (article the) (noun class))
;         (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))
;       )
;     )
;   )
; )
; 教授は、講義する
;         ----
;           |__to 学生
;           |__in 教室
;                 ----
;                   |__with 猫
; 教授は学生に講義する。教授は教室で講義する。教室には猫がいる。

; (sentence 
;   (simple-noun-phrase (article the) (noun professor))
;   (verb-phrase 
;     (verb-phrase 
;       (verb lectures)
;       (prep-phrase 
;         (prep to)
;         (noun-phrase 
;           (simple-noun-phrase (article the) (noun student))
;           (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class)))
;         )
;       )
;     )
;     (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))
;   )
; )
; 教授は、講義する
;         ----
;           |__to 学生
;           |     ----
;           |       |__in 教室
;           |__with 猫
; 教授は学生に講義する。学生は教室にいる。（教授が教室にいるとは限らない。例えばリモート講義など）講義は猫と協力して行う。

; (sentence 
;   (simple-noun-phrase (article the) (noun professor))
;   (verb-phrase 
;     (verb lectures)
;     (prep-phrase 
;       (prep to)
;       (noun-phrase 
;         (noun-phrase 
;           (simple-noun-phrase (article the) (noun student))
;           (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class)))
;         )
;         (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))
;       )
;     )
;   )
; )
; 教授は、講義する
;         ----
;           |__to 学生
;                 ----
;                   |__in 教室
;                   |__with 猫
; 教授は学生に講義する。学生は教室にいる。（教授が教室にいるとは限らない）学生は猫とともにいる。

; (sentence 
;   (simple-noun-phrase (article the) (noun professor))
;   (verb-phrase 
;     (verb lectures)
;     (prep-phrase 
;       (prep to)
;       (noun-phrase 
;         (simple-noun-phrase (article the) (noun student))
;         (prep-phrase 
;           (prep in)
;           (noun-phrase 
;             (simple-noun-phrase (article the) (noun class))
;             (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))
;           )
;         )
;       )
;     )
;   )
; )
; 教授は、講義する
;         ----
;           |__to 学生
;                 ----
;                   |__in 教室
;                         ----
;                           |__with 猫
; 教授は学生に講義する。学生は教室にいる。教室には猫がいる。（学生と猫は一緒にいるとは限らない。お互いが教室内の離れた位置にいるなど）
