#lang racket
(provide (all-defined-out))

(require sicp)

(define 
  (install-eval-apply repl)

  (repl 
    '( ;
      ; eval
      (define 
        (qeval query frame)
        (let 
          ( ;
           (tag (type query))
           (contents (contents query))
          )

          (cond 
            ((eq? tag 'and) (conjoin contents frame))
            ((eq? tag 'or) (disjoin contents frame))
            ((eq? tag 'not) (negate contents frame))
            ((eq? tag 'lisp-value) (lisp-value contents frame))
            ((eq? tag 'always-true) (always-true contents frame))
            (else (simple-query query frame))
          )
        )
      )

      (define 
        (simple-query query-pattern frame)
        (amb 
          (find-assertion query-pattern frame)
          (apply-rule query-pattern frame)
        )
      )

      (define 
        (conjoin conjuncts frame)
        (if (empty-conjunction? conjuncts) 
          frame
          (conjoin 
            (rest-conjuncts conjuncts)
            (qeval (first-conjunct conjuncts) frame)
          )
        )
      )

      (define 
        (disjoin disjuncts frame)
        (require (not (empty-disjunction? disjuncts)))
        (amb 
          (qeval (first-disjunct disjuncts) frame)
          (disjoin (rest-disjuncts disjuncts) frame)
        )
      )

      (define 
        (negate operands frame)
        (let 
          ( ;
           (result 
             (try-catch 
               (qeval 
                 (negated-query operands)
                 frame
               )
               'failed
             )
           )
          )

          (require (eq? result 'failed))
          frame
        )
      )

      (define 
        (lisp-value call frame)

        (define 
          (execute exp)
          (scheme-apply 
            (scheme-eval (predicate exp) (scheme-make-base-namespace))
            (args exp)
          )
        )

        (let 
          ( ;
           (result 
             (execute 
               (instantiate 
                 call
                 frame
                 (lambda (v f) 
                   (error "Unknown pat var: LISP-VALUE" v)
                 )
               )
             )
           )
          )

          (require result)
          frame
        )
      )

      (define 
        (always-true ignore frame)
        frame
      )

      ; apply
      ;; pattern-match
      (define 
        (find-assertion pattern frame)
        (check-an-assertion (fetch-assertion) pattern frame)
      )

      (define 
        (check-an-assertion assertion query-pat query-frame)
        (let 
          ( ;
           (match-result 
             (pattern-match query-pat assertion query-frame)
           )
          )

          (require (not (eq? match-result 'failed)))
          match-result
        )
      )

      (define 
        (pattern-match pat dat frame)
        (cond 
          ((eq? frame 'failed) 'failed)
          ((equal? pat dat) frame)
          ((var? pat) (extend-if-consistent pat dat frame))
          ((and (pair? pat) (pair? dat))
           (pattern-match 
             (cdr pat)
             (cdr dat)
             (pattern-match (car pat) (car dat) frame)
           )
          )
          (else 'failed)
        )
      )

      (define 
        (extend-if-consistent var dat frame)
        (let 
          ( ;
           (binding (binding-in-frame var frame))
          )

          (if binding 
            (pattern-match (binding-value binding) dat frame)
            (extend var dat frame)
          )
        )
      )

      ;; unification
      (define 
        (apply-rule pattern frame)
        (apply-a-rule 
          (fetch-rule)
          pattern
          frame
        )
      )

      (define 
        (apply-a-rule rule query-pattern query-frame)
        (let 
          ( ;
           (clean-rule (rename-variables-in rule))
          )

          (let 
            ( ;
             (unify-result 
               (unify-match 
                 query-pattern
                 (conclusion clean-rule)
                 query-frame
               )
             )
            )

            (require (not (eq? unify-result 'failed)))
            (qeval 
              (rule-body clean-rule)
              unify-result
            )
          )
        )
      )

      (define 
        (rename-variables-in rule)
        (let 
          ( ;
           (rule-application-id (new-rule-application-id))
          )

          (define 
            (tree-walk exp)
            (cond 
              ((var? exp)
               (make-new-variable exp rule-application-id)
              )
              ((pair? exp)
               (cons 
                 (tree-walk (car exp))
                 (tree-walk (cdr exp))
               )
              )
              (else exp)
            )
          )

          (tree-walk rule)
        )
      )

      (define 
        (unify-match p1 p2 frame)
        (cond 
          ((eq? frame 'failed) 'failed)
          ((equal? p1 p2) frame)
          ((var? p1) (extend-if-possible p1 p2 frame))
          ((var? p2) (extend-if-possible p2 p1 frame))
          ((and (pair? p1) (pair? p2))
           (unify-match 
             (cdr p1)
             (cdr p2)
             (unify-match 
               (car p1)
               (car p2)
               frame
             )
           )
          )
          (else 'failed)
        )
      )

      (define 
        (extend-if-possible var val frame)
        (let 
          ( ;
           (binding (binding-in-frame var frame))
          )

          (cond 
            (binding
             (unify-match (binding-value binding) val frame)
            )
            ((var? val)
             (let 
               ( ;
                (binding (binding-in-frame val frame))
               )

               (if binding 
                 (unify-match 
                   var
                   (binding-value binding)
                   frame
                 )
                 (extend var val frame)
               )
             )
            )
            ((depends-on? val var frame)
             'failed
            )
            (else (extend var val frame))
          )
        )
      )

      (define 
        (depends-on? exp var frame)
        (define 
          (tree-walk e)
          (cond 
            ((var? e)
             (if (equal? var e) 
               true
               (let 
                 ( ;
                  (b (binding-in-frame e frame))
                 )

                 (if b 
                   (tree-walk (binding-value b))
                   false
                 )
               )
             )
            )
            ((pair? e)
             (or 
               (tree-walk (car e))
               (tree-walk (cdr e))
             )
            )
            (else false)
          )
        )
        (tree-walk exp)
      )

      ; utils
      (define 
        (instantiate exp frame unbound-var-handler)

        (define 
          (copy exp)
          (cond 
            ((var? exp)
             (let 
               ( ;
                (binding (binding-in-frame exp frame))
               )

               (if binding 
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame)
               )
             )
            )
            ((pair? exp)
             (cons (copy (car exp)) (copy (cdr exp)))
            )
            (else exp)
          )
        )

        (copy exp)
      )
     )
  )
)
