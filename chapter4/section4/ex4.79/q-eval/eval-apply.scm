#lang racket
(provide (all-defined-out))

(require sicp)
(require "data-directed-utils.scm")
(require "stream.scm")
(require "expressions.scm")
(require "environment.scm")

; eval
(define 
  (qeval query world-stream)
  (let 
    ( ;
     (qproc (get (type query) 'qeval))
    )

    (if qproc 
      (qproc (contents query) world-stream)
      (simple-query query world-stream)
    )
  )
)

(define 
  (simple-query query world-stream)
  (stream-flatmap 
    (lambda (world) 
      (stream-append-delayed 
        (find-assertions query world)
        (delay (apply-rules query world))
      )
    )
    world-stream
  )
)

(define 
  (add-assertion-to-world-stream assert world-stream)
  (stream-map 
    (lambda (world) 
      (add-assertion 
        assert
        (world-current-environment-id world)
        world
      )
    )
    world-stream
  )
)
(define 
  (add-rule-to-world-stream rule world-stream)
  (stream-map 
    (lambda (world) 
      (add-rule 
        rule
        (world-current-environment-id world)
        world
      )
    )
    world-stream
  )
)
(define 
  (assertion operand world-stream)
  (let 
    ( ;
     (body (car operand))
    )

    (if (eq? (car body) 'rule) 
      (add-rule-to-world-stream body world-stream)
      (add-assertion-to-world-stream body world-stream)
    )
  )
)
(put 'assert! 'qeval assertion)

(define 
  (conjoin conjuncts world-stream)
  (if (empty-conjunction? conjuncts) 
    world-stream
    (conjoin 
      (rest-conjuncts conjuncts)
      (qeval (first-conjunct conjuncts) world-stream)
    )
  )
)
(put 'and 'qeval conjoin)

(define 
  (disjoin disjuncts world-stream)
  (if (empty-disjunction? disjuncts) 
    internal-the-empty-stream
    (interleave-delayed 
      (qeval (first-disjunct disjuncts) world-stream)
      (delay (disjoin (rest-disjuncts disjuncts) world-stream))
    )
  )
)
(put 'or 'qeval disjoin)

(define 
  (negate operands world-stream)
  (stream-flatmap 
    (lambda (world) 
      (if 
        (internal-stream-null? 
          (qeval 
            (set-current-env-id (negated-query operands) world)
            (singleton-stream world)
          )
        )
        (singleton-stream world)
        internal-the-empty-stream
      )
    )
    world-stream
  )
)
(put 'not 'qeval negate)

(define 
  (lisp-value call world-stream)

  (define 
    (execute exp)
    (apply 
      (eval (predicate exp) (make-base-namespace))
      (args exp)
    )
  )

  (stream-flatmap 
    (lambda (world) 
      (if 
        (execute 
          (instantiate 
            (set-current-env-id call world)
            world
            (lambda (v w) (error "Unknown pat var: LISP-VALUE" v))
          )
        )
        (singleton-stream world)
        internal-the-empty-stream
      )
    )
    world-stream
  )
)
(put 'lisp-value 'qeval lisp-value)

(define (always-true ignore world-stream) world-stream)
(put 'always-true 'qeval always-true)

; apply
;; pattern match
(define 
  (find-assertions query world)
  (stream-flatmap 
    (lambda (assertion) 
      (check-an-assertion assertion (set-current-env-id query world) world)
    )
    (lookup-all-assertions (world-current-environment-id world) world)
  )
)

(define 
  (check-an-assertion assertion query world)
  (let 
    ( ;
     (match-result 
       (pattern-match 
         (set-env-id-to-variables-in 
           query
           (world-current-environment-id world)
         )
         assertion
         world
       )
     )
    )

    (if (eq? match-result 'failed) 
      internal-the-empty-stream
      (singleton-stream match-result)
    )
  )
)

(define 
  (pattern-match query data world)
  (cond 
    ((eq? world 'failed) 'failed)
    ((equal? query data) world)
    ((var? query) (bind-if-consistent query data world))
    ((and (pair? query) (pair? data))
     (pattern-match 
       (cdr query)
       (cdr data)
       (pattern-match (car query) (car data) world)
     )
    )
    (else 'failed)
  )
)

(define 
  (bind-if-consistent var data world)
  (let 
    ( ;
     (binding (lookup-binding var world))
    )

    (if binding 
      (pattern-match 
        (binding-value binding)
        data
        world
      )
      (bind var data world)
    )
  )
)

;; unification
(define 
  (apply-rules query world)
  (stream-flatmap 
    (lambda (rule) 
      (apply-a-rule rule (set-current-env-id query world) world)
    )
    (lookup-all-rules (world-current-environment-id world) world)
  )
)

(define 
  (apply-a-rule rule query world)
  (let 
    ( ;
     (query-env-id (world-current-environment-id world))
     (extended-world (extend world))
    )

    (let 
      ( ;
       (unify-result 
         (unify-match 
           query
           (set-current-env-id (conclusion rule) extended-world)
           extended-world
         )
       )
      )

      (if (eq? unify-result 'failed) 
        internal-the-empty-stream
        (stream-map 
          (lambda (world) 
            (set-current-environment-id query-env-id world)
          )
          (qeval 
            (set-current-env-id (rule-body rule) extended-world)
            (singleton-stream unify-result)
          )
        )
      )
    )
  )
)

(define 
  (unify-match p1 p2 world)
  (cond 
    ((eq? world 'failed) 'failed)
    ((equal? p1 p2) world)
    ((var? p1) (bind-if-possible p1 p2 world))
    ((var? p2) (bind-if-possible p2 p1 world))
    ((and (pair? p1) (pair? p2))
     (unify-match 
       (cdr p1)
       (cdr p2)
       (unify-match 
         (car p1)
         (car p2)
         world
       )
     )
    )
    (else 'failed)
  )
)

(define 
  (bind-if-possible var val world)
  (let 
    ( ;
     (binding (lookup-binding var world))
    )

    (cond 
      (binding
       (unify-match (binding-value binding) val world)
      )
      ((var? val)
       (let 
         ( ;
          (binding (lookup-binding val world))
         )

         (if binding 
           (unify-match 
             var
             (binding-value binding)
             world
           )
           (bind var val world)
         )
       )
      )
      ((depends-on? val var world)
       'failed
      )
      (else (bind var val world))
    )
  )
)

(define 
  (depends-on? exp var world)

  (define 
    (tree-walk exp)
    (cond 
      ((var? exp)
       (if (equal? var exp) 
         true
         (let 
           ( ;
            (binding (lookup-binding exp world))
           )

           (if binding 
             (tree-walk (binding-value binding))
             false
           )
         )
       )
      )
      ((pair? exp)
       (or 
         (tree-walk (car exp))
         (tree-walk (cdr exp))
       )
      )
      (else false)
    )
  )

  (tree-walk exp)
)

; utils
(define 
  (instantiate exp world unbound-var-handler)

  (define 
    (copy exp)
    (cond 
      ((var? exp)
       (let 
         ( ;
          (binding (lookup-binding exp world))
         )

         (if binding 
           (let 
             ( ;
              (value 
                (lookup-binding-value 
                  (binding-value binding)
                  world
                )
              )
             )

             (if (not (eq? value 'failed)) 
               (copy value)
               (unbound-var-handler exp world)
             )
           )
           (unbound-var-handler exp world)
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

(define 
  (set-env-id-to-variables-in exp env-id)

  (define 
    (tree-walk exp)
    (cond 
      ((var? exp)
       (set-variable-environment-id exp env-id)
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

  (tree-walk exp)
)

(define 
  (set-current-env-id exp world)

  (define env-id (world-current-environment-id world))

  (define 
    (tree-walk exp)
    (cond 
      ((var? exp)
       (set-variable-environment-id exp env-id)
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

  (tree-walk exp)
)
