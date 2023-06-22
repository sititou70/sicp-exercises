#lang racket
(provide (all-defined-out))

(require sicp)
(require "data-directed-utils.scm")
(require "stream.scm")

; variable: (list '? environment-id symbol)
(define (var? exp) (tagged-list? exp '?))
(define (make-variable environment-id symbol) (list '? environment-id symbol))
(define (variable-environment-id var) (cadr var))
(define (variable-symbol var) (caddr var))

; binding: (variable . value)
;   value: 定数もしくはvariable
;     定数: その定数値そのもの
;     変数: (環境 . 変数)
(define 
  (make-binding variable value)
  (cons variable value)
)
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))

; environment: (list id bindings assertions rules parent)
;   id: 各環境で一意な値
;   bindings: bindingのリスト
;   assertions: assertionのストリーム。assertionの例：(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
;   rules: ruleのストリーム。ruleの例：(rule (same ?x ?x))
;   parent: 親の環境のid。ない場合は'()
(define 
  (make-environment id bindings assertions rules parent)
  (list id bindings assertions rules parent)
)
(define 
  (environment-id env)
  (car env)
)
(define 
  (environment-bindings env)
  (cadr env)
)
(define 
  (environment-assertions env)
  (caddr env)
)
(define 
  (environment-rules env)
  (cadddr env)
)
(define 
  (environment-parent env)
  (car (cddddr env))
)

; world: (environmentのリスト . カレント環境のid)
(define 
  (make-world environments current-environment-id)
  (cons environments current-environment-id)
)
(define (world-environments world) (car world))
(define (world-current-environment-id world) (cdr world))

; utils
(define 
  (get-environment env-id world)
  (assoc env-id (world-environments world))
)
(define 
  (set-environment env world)

  (define 
    (set-envs envs)
    (if (null? envs) 
      (cons env envs)
      (if (eq? (environment-id env) (environment-id (car envs))) 
        (cons 
          env
          (cdr envs)
        )
        (cons 
          (car envs)
          (set-envs (cdr envs))
        )
      )
    )
  )

  (make-world 
    (set-envs (world-environments world))
    (world-current-environment-id world)
  )
)

(define 
  (bind var val world)

  (define 
    (bind-in-environment var val env)
    (make-environment 
      (environment-id env)
      (cons (make-binding var val) (environment-bindings env))
      (environment-assertions env)
      (environment-rules env)
      (environment-parent env)
    )
  )

  (set-environment 
    (bind-in-environment 
      var
      val
      (get-environment 
        (variable-environment-id var)
        world
      )
    )
    world
  )
)

(define *ENVIRONMENT-ID* 0)
(define 
  (new-environment-id)
  (set! *ENVIRONMENT-ID* (+ *ENVIRONMENT-ID* 1))
  *ENVIRONMENT-ID*
)
(define 
  (extend world)
  (let 
    ( ;
     (new-env 
       (make-environment 
         (new-environment-id)
         '()
         internal-the-empty-stream
         internal-the-empty-stream
         (world-current-environment-id world)
       )
     )
    )

    (make-world 
      (cons new-env (world-environments world))
      (environment-id new-env)
    )
  )
)
(define 
  (new-empty-world)
  (extend (make-world '() '()))
)

(define 
  (add-assertion assertion env-id world)

  (define 
    (add-assertion-to-environment assertion env)
    (make-environment 
      (environment-id env)
      (environment-bindings env)
      (internal-stream-cons assertion (environment-assertions env))
      (environment-rules env)
      (environment-parent env)
    )
  )

  (set-environment 
    (add-assertion-to-environment 
      assertion
      (get-environment env-id world)
    )
    world
  )
)

(define 
  (add-rule rule env-id world)

  (define 
    (add-rule-to-environment rule env)
    (make-environment 
      (environment-id env)
      (environment-bindings env)
      (environment-assertions env)
      (internal-stream-cons rule (environment-rules env))
      (environment-parent env)
    )
  )

  (set-environment 
    (add-rule-to-environment 
      rule
      (get-environment env-id world)
    )
    world
  )
)

(define 
  (lookup-all-assertions env-id world)
  (if (null? env-id) 
    internal-the-empty-stream
    (let 
      ( ;
       (env (get-environment env-id world))
      )

      (if (null? env) 
        internal-the-empty-stream
        (stream-append 
          (environment-assertions env)
          (lookup-all-assertions 
            (environment-parent env)
            world
          )
        )
      )
    )
  )
)
(define 
  (lookup-all-rules env-id world)
  (if (null? env-id) 
    internal-the-empty-stream
    (let 
      ( ;
       (env (get-environment env-id world))
      )

      (if (null? env) 
        internal-the-empty-stream
        (stream-append 
          (environment-rules env)
          (lookup-all-rules 
            (environment-parent env)
            world
          )
        )
      )
    )
  )
)

(define 
  (binding-in-environment var env)
  (assoc var (environment-bindings env))
)
(define 
  (lookup-binding var world)

  (define 
    (lookup env-id)
    (if (null? env-id) 
      #f
      (let 
        ( ;
         (env 
           (get-environment env-id world)
         )
        )

        (if (null? env) 
          #f
          (let 
            ( ;
             (binding (binding-in-environment var env))
            )

            (if binding 
              binding
              (lookup (environment-parent env))
            )
          )
        )
      )
    )
  )

  (lookup (variable-environment-id var))
)
(define 
  (lookup-binding-value val world)
  (if (var? val) 
    (let 
      ( ;
       (env 
         (get-environment 
           (variable-environment-id val)
           world
         )
       )
      )

      (if (null? env) 
        'failed
        (let 
          ( ;
           (binding 
             (binding-in-environment val env)
           )
          )

          (if binding 
            (lookup-binding-value (binding-value binding) world)
            'failed
          )
        )
      )
    )
    val
  )
)

(define 
  (set-variable-environment-id var env-id)
  (make-variable env-id (variable-symbol var))
)

(define 
  (set-current-environment-id env-id world)
  (make-world 
    (world-environments world)
    env-id
  )
)
