Louis の以下の提案はうまく動作しない

```scheme
(define
  (parse-verb-phrase)
  (amb
    (parse-word verbs)
    (list 'verb-phrase
          (parse-verb-phrase)
          (parse-prepositional-phrase)
    )
  )
)
```

`(parse-verb-phrase)`は単一の amb に評価されるため、一番最初に`(parse-word verbs)`が評価される。ここでもし`(parse-word verbs)`が失敗する場合、amb の 2 つ目の式が常に評価され、そこでは`(parse-verb-phrase)`が呼び出されるため、無限ループになってしまい停止しない。

オリジナルの手続きでは、

```scheme
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
```

`(parse-word verbs)`が失敗する場合、`maybe-extend`が呼び出されないためそこで停止していた。

以下のように amb の式の順番を入れ替える場合は、

```scheme
(define
  (parse-verb-phrase)
  (amb
    (list 'verb-phrase
          (parse-verb-phrase)
          (parse-prepositional-phrase)
    )
    (parse-word verbs)
  )
)
```

`(parse-word verbs)`の成功 / 失敗にかかわらず無限ループするようになってしまう。amb の最初の式で`(parse-verb-phrase)`を呼び出しているからである。
