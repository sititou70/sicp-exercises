我々の構文解析器が定義する構文は、被演算子の評価順序に依存している。例えば

```scheme
(list
  'simple-sentence
  (parse-noun-phrase)
  (parse-verb-phrase)
)
```

という式は、list の被演算子である(parse-noun-phrase)が評価されてから(parse-verb-phrase)が評価されることを意図している。これは、名詞句の次に動詞句が続くという構文に対応している。

したがって、被演算子の評価順序が違うものであったら、構文解析器は全く違う構文に対応したものとなり、うまく動作しなくなってしまう。
