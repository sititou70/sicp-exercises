# a. 3 つの取引が完了した後で balance が取りうる異なる値をすべて列挙せよ。

Peter、Paul、Mary のプロセスが実行される順番と、実行後の balance は以下のようになる。

- 順番：balance
- Peter、Paul、Mary：45
- Peter、Mary、Paul：35
- Paul、Peter、Mary：45
- Paul、Mary、Peter：50
- Mary、Peter、Paul：40
- Mary、Paul、Peter：40

balance が取りうる値は、35、40、45、50。

# b. もしシステムがプロセスの実行が入れ違いになることを許すとすると、ほかにどのような値になる可能性があるだろうか。

例えば、balance が 110 になることがある。

- Peter: Access balance, $100
- Paul: Access balance, $100
- Mary: Access balance, $100
- Paul: New value, 100 - 20 = 80
- Paul: set! balance to $80
- Mary: New value, 100 - (100 / 2) = 50
- Mary: set! balance to $50
- Peter: New value, 100 + 10 = 110
- Peter: set! balance to $110
