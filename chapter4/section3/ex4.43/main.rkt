#lang racket

(require (rename-in sicp [apply sicp-apply] [eval sicp-eval]))
(require "m-eval/eval-apply.rkt")
(require "m-eval/global-environment.rkt")

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
  '(define
    (require p)
    (if (not p) (amb))
   )
)

(repl 
  '(define
    (distinct? items)
    (cond 
      ((null? items) true)
      ((null? (cdr items)) true)
      ((memq (car items) (cdr items)) false)
      (else (distinct? (cdr items)))
    )
   )
)

(repl 
  '(define
    (implies p1 p2)
    (if p1 p2 true)
   )
)

; 問題文を整理すると以下のようになる
; 父親
; moore
; downing
; hall
; barnacle
; parker
; 
; 父親：クルーザー（他の誰かの娘の名前）
; barnacle：gabrielle
; moore：lorna
; hall：rosalind
; downing：melissa
; gabrielleの⽗親：parkerの娘の名前
; 
; 父親：娘
; barnacle：melissa
; moore：mary
(repl 
  '(define
    (liars-puzzle)

    ; 父親：クルーザー
    (define moore-cruiser (amb 'gabrielle 'lorna 'rosalind 'melissa 'mary))
    (require (eq? moore-cruiser 'lorna))

    (define downing-cruiser (amb 'gabrielle 'lorna 'rosalind 'melissa 'mary))
    (require (eq? downing-cruiser 'melissa))

    (define hall-cruiser (amb 'gabrielle 'lorna 'rosalind 'melissa 'mary))
    (require (eq? hall-cruiser 'rosalind))

    (define barnacle-cruiser (amb 'gabrielle 'lorna 'rosalind 'melissa 'mary))
    (require (eq? barnacle-cruiser 'gabrielle))

    (define parker-cruiser (amb 'gabrielle 'lorna 'rosalind 'melissa 'mary))

    ; 父親：娘
    (define moore-daughter (amb 'gabrielle 'lorna 'rosalind 'melissa 'mary))
    (require (eq? moore-daughter 'mary))

    (define downing-daughter (amb 'gabrielle 'lorna 'rosalind 'melissa 'mary))
    (define hall-daughter (amb 'gabrielle 'lorna 'rosalind 'melissa 'mary))
    (define barnacle-daughter (amb 'gabrielle 'lorna 'rosalind 'melissa 'mary))
    (require (eq? barnacle-daughter 'melissa))

    (define parker-daughter (amb 'gabrielle 'lorna 'rosalind 'melissa 'mary))

    ; 追加の制約
    (require (not (eq? moore-cruiser moore-daughter)))
    (require (not (eq? downing-cruiser downing-daughter)))
    (require (not (eq? hall-cruiser hall-daughter)))
    (require (not (eq? barnacle-cruiser barnacle-daughter)))
    (require (not (eq? parker-cruiser parker-daughter)))

    (require 
      (distinct? 
        (list moore-cruiser downing-cruiser hall-cruiser barnacle-cruiser 
              parker-cruiser
        )
      )
    )
    (require 
      (distinct? 
        (list moore-daughter downing-daughter hall-daughter barnacle-daughter 
              parker-daughter
        )
      )
    )

    (require 
      (implies 
        (eq? moore-daughter 'gabrielle)
        (eq? moore-cruiser parker-daughter)
      )
    )
    (require 
      (implies 
        (eq? downing-daughter 'gabrielle)
        (eq? downing-cruiser parker-daughter)
      )
    )
    (require 
      (implies 
        (eq? hall-daughter 'gabrielle)
        (eq? hall-cruiser parker-daughter)
      )
    )
    (require 
      (implies 
        (eq? barnacle-daughter 'gabrielle)
        (eq? barnacle-cruiser parker-daughter)
      )
    )
    (require 
      (implies 
        (eq? parker-daughter 'gabrielle)
        (eq? parker-cruiser parker-daughter)
      )
    )

    (list 
      (list 
        (list 'moore-cruiser moore-cruiser)
        (list 'downing-cruiser downing-cruiser)
        (list 'hall-cruiser hall-cruiser)
        (list 'barnacle-cruiser barnacle-cruiser)
        (list 'parker-cruiser parker-cruiser)
      )
      (list 
        (list 'moore-daughter moore-daughter)
        (list 'downing-daughter downing-daughter)
        (list 'hall-daughter hall-daughter)
        (list 'barnacle-daughter barnacle-daughter)
        (list 'parker-daughter parker-daughter)
      )
    )
   )
)

(repl '(liars-puzzle))
(repl 'try-again)
; {{{moore-cruiser lorna} {downing-cruiser melissa} {hall-cruiser rosalind} {barnacle-cruiser gabrielle} {parker-cruiser mary}} {{moore-daughter mary} {downing-daughter lorna} {hall-daughter gabrielle} {barnacle-daughter melissa} {parker-daughter rosalind}}}
;;; There are no more values of: (liars-puzzle)

; lornaの父親はdowning

(repl 
  '(define
    (liars-puzzle-alt)

    ; 父親：クルーザー
    (define moore-cruiser (amb 'gabrielle 'lorna 'rosalind 'melissa 'mary))
    (require (eq? moore-cruiser 'lorna))

    (define downing-cruiser (amb 'gabrielle 'lorna 'rosalind 'melissa 'mary))
    (require (eq? downing-cruiser 'melissa))

    (define hall-cruiser (amb 'gabrielle 'lorna 'rosalind 'melissa 'mary))
    (require (eq? hall-cruiser 'rosalind))

    (define barnacle-cruiser (amb 'gabrielle 'lorna 'rosalind 'melissa 'mary))
    (require (eq? barnacle-cruiser 'gabrielle))

    (define parker-cruiser (amb 'gabrielle 'lorna 'rosalind 'melissa 'mary))

    ; 父親：娘
    (define moore-daughter (amb 'gabrielle 'lorna 'rosalind 'melissa 'mary))
    (define downing-daughter (amb 'gabrielle 'lorna 'rosalind 'melissa 'mary))
    (define hall-daughter (amb 'gabrielle 'lorna 'rosalind 'melissa 'mary))

    (define barnacle-daughter (amb 'gabrielle 'lorna 'rosalind 'melissa 'mary))
    (require (eq? barnacle-daughter 'melissa))

    (define parker-daughter (amb 'gabrielle 'lorna 'rosalind 'melissa 'mary))

    ; 追加の制約
    (require (not (eq? moore-cruiser moore-daughter)))
    (require (not (eq? downing-cruiser downing-daughter)))
    (require (not (eq? hall-cruiser hall-daughter)))
    (require (not (eq? barnacle-cruiser barnacle-daughter)))
    (require (not (eq? parker-cruiser parker-daughter)))

    (require 
      (distinct? 
        (list moore-cruiser downing-cruiser hall-cruiser barnacle-cruiser 
              parker-cruiser
        )
      )
    )
    (require 
      (distinct? 
        (list moore-daughter downing-daughter hall-daughter barnacle-daughter 
              parker-daughter
        )
      )
    )

    (require 
      (implies 
        (eq? moore-daughter 'gabrielle)
        (eq? moore-cruiser parker-daughter)
      )
    )
    (require 
      (implies 
        (eq? downing-daughter 'gabrielle)
        (eq? downing-cruiser parker-daughter)
      )
    )
    (require 
      (implies 
        (eq? hall-daughter 'gabrielle)
        (eq? hall-cruiser parker-daughter)
      )
    )
    (require 
      (implies 
        (eq? barnacle-daughter 'gabrielle)
        (eq? barnacle-cruiser parker-daughter)
      )
    )
    (require 
      (implies 
        (eq? parker-daughter 'gabrielle)
        (eq? parker-cruiser parker-daughter)
      )
    )

    (list 
      (list 
        (list 'moore-cruiser moore-cruiser)
        (list 'downing-cruiser downing-cruiser)
        (list 'hall-cruiser hall-cruiser)
        (list 'barnacle-cruiser barnacle-cruiser)
        (list 'parker-cruiser parker-cruiser)
      )
      (list 
        (list 'moore-daughter moore-daughter)
        (list 'downing-daughter downing-daughter)
        (list 'hall-daughter hall-daughter)
        (list 'barnacle-daughter barnacle-daughter)
        (list 'parker-daughter parker-daughter)
      )
    )
   )
)

(repl '(liars-puzzle-alt))
(repl 'try-again)
(repl 'try-again)
; {{{moore-cruiser lorna} {downing-cruiser melissa} {hall-cruiser rosalind} {barnacle-cruiser gabrielle} {parker-cruiser mary}} {{moore-daughter gabrielle} {downing-daughter rosalind} {hall-daughter mary} {barnacle-daughter melissa} {parker-daughter lorna}}}
; {{{moore-cruiser lorna} {downing-cruiser melissa} {hall-cruiser rosalind} {barnacle-cruiser gabrielle} {parker-cruiser mary}} {{moore-daughter mary} {downing-daughter lorna} {hall-daughter gabrielle} {barnacle-daughter melissa} {parker-daughter rosalind}}}
;;; There are no more values of: (liars-puzzle-alt)

; 解は2通りに増える
