`'acquire`メッセージが 2 つ並行に発行されたとする。すると、以下のようなプロセス x、y が実行される。現状のセルが false だとすると、

- x1：cell の中身を確かめる：false であった
- y1：cell の中身を確かめる：false であった
- x2：cell の中身を true にして、acquire は完了する
- y2：cell の中身を true にして、acquire は完了する

結果として、ミューテックスが 2 つ獲得されてしまった。
