もし Ben が重要人物（wheel）の給料の合計を求めようとすると間違った答えになってしまう。

例えば以下のようなクエリは

```scheme
(sum
  ?amount
  (and
    (wheel ?person)
    (salary ?person ?amount)
  )
)
```

以下のクエリの結果における amount を合計する。

クエリ：

```scheme
(and
  (wheel ?person)
  (salary ?person ?amount)
)
```

結果：

```text
{and {wheel {Warbucks Oliver}} {salary {Warbucks Oliver} 150000}}
{and {wheel {Warbucks Oliver}} {salary {Warbucks Oliver} 150000}}
{and {wheel {Bitdiddle Ben}} {salary {Bitdiddle Ben} 60000}}
{and {wheel {Warbucks Oliver}} {salary {Warbucks Oliver} 150000}}
{and {wheel {Warbucks Oliver}} {salary {Warbucks Oliver} 150000}}
```

しかし、Warbucks Oliver は複数回表示されてしまっているので、間違った合計結果が得られてしまう。

状況を打開するには、結果のストリームを特定のキーで重複排除してから合計できるようにする。例えば

```scheme
(sum
  ?amount
  (and
    (wheel ?person)
    (salary ?person ?amount)
  )
  distinct ?person
)
```

のような形式を受け入れるようにする。

具体的な distinct の実装については以下を参照

https://www.serendip.ws/archives/2719
