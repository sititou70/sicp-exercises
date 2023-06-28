以下の階乗計算マシンで n が 2 の場合を手計算でシミュレートする

```scheme
(controller
  (assign continue (label fact-done)) ;set up final return address

  fact-loop
  (test (op =) (reg n) (const 1))
  (branch (label base-case))
  ;; Set up for the recursive call by saving n and continue.
  ;; Set up continue so that the computation will continue
  ;; at after-fact when the subroutine returns.
  (save continue)
  (save n)
  (assign n (op -) (reg n) (const 1))
  (assign continue (label after-fact))
  (goto (label fact-loop))

  after-fact
  (restore n)
  (restore continue)
  (assign val (op *) (reg n) (reg val)) ;val now contains n(n - 1)!
  (goto (reg continue)) ;return to caller

  base-case
  (assign val (const 1)) ;base case: 1! = 1
  (goto (reg continue)) ;return to caller

  fact-done
)
```

1. continue に fact-done がセットされる
1. n == 1 がテストされる。結果は偽
1. 直前のテストの結果が偽であるため、ブランチ命令は何もしない
1. continue と n が save される。ここでスタックは`[2, fact-done]`となる。右が先に取り出される要素である
1. n <- 1、continue <- after-fact がアサインされる
1. fact-loop に goto する
1. n == 1 がテストされる。結果は真
1. 直前の結果が真であるため、ブランチ命令によって base-case に goto する
1. val <- 1 がアサインされる
1. contiune、つまり after-fact に goto する
1. n と continue が`[2, fact-done]`から restore される。スタックの中身は`[]`になる
1. val <- n \* val がアサインされ、val は 2 になる。
1. continue、つまり fact-done に goto する

次に、以下のフィボナッチマシンを n が 2 の場合について手作業でシミュレートする

```scheme
(controller
  (assign continue (label fib-done))

  fib-loop
  (test (op <) (reg n) (const 2))
  (branch (label immediate-answer))

  ;; Fib(n-1) を求める準備
  (save continue)
  (assign continue (label afterfib-n-1))
  (save n) ; n の古い値を保存
  (assign n (op -) (reg n) (const 1)) ; n を n-1 で上書き
  (goto (label fib-loop)) ; 再帰呼び出しの実⾏

  afterfib-n-1 ; リターン時に Fib(n-1) は val に⼊っている
  (restore n)
  (restore continue)

  ;; Fib(n-2) を求める準備
  (assign n (op -) (reg n) (const 2))
  (save continue)
  (assign continue (label afterfib-n-2))
  (save val) ; Fib(n-1) を保存
  (goto (label fib-loop))

  afterfib-n-2 ; リターン時に Fib(n-2) は val に⼊っている
  (assign n (reg val)) ; n には Fib(n-2) が⼊る
  (restore val) ; val には Fib(n-1) が⼊る
  (restore continue)
  (assign
    val ; Fib(n-1) + Fib(n-2)
    (op +)
    (reg val)
    (reg n)
  )
  (goto (reg continue)) ; 呼び出し元に戻る、答えはvalの中

  immediate-answer
  (assign val (reg n)) ; 基底の場合: Fib(n) = n
  (goto (reg continue))

  fib-done
)
```

1. continut <- fib-done
1. n < 2 をテスト、n は 2 なので結果は偽
1. 直前の結果が偽であるためブランチ命令はなにもしない
1. contunue を save。stack は`[fib-done]`
1. continue <- afterfib-n-1
1. n を save。stack は`[2, fib-done]`
1. n <- n - 1。n は 1 になる。
1. fib-loop に goto して再帰呼出し実行
1. n < 2 をテスト、n は 1 なので結果は真
1. ブランチ命令によって immediate-answer に goto
1. val <- n、val は 1
1. continue、つまり afterfib-n-1 に goto
1. n と continue を`[2, fib-done]`から復元。stack は`[]`に
1. n <- n - 2。n は 0 になる。
1. continue を save。stack は`[fib-done]`に
1. continue <- afterfib-n-2
1. val を save。stack は`[1, fib-done]`に
1. fib-loop に goto して再帰呼出し実行
1. n < 2 をテスト、n は 0 なので結果は真
1. ブランチ命令によって immediate-answer に goto
1. val <- n、val は 0
1. continue、つまり afterfib-n-2 に goto
1. n <- val、n は 0 に
1. val を`[1, fib-done]`から復元。stack は`[fib-done]`に
1. continue を`[fib-done]`から復元。stack は`[]`に
1. val <- val + n、val は 1 に
1. continue、つまり fib-done に goto
