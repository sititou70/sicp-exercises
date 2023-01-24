うまく行かない．Alyssa が考えた`expmod`は，`base^exp`を計算してから`m`での剰余を求める．ここで，`base^exp`はとても大きな数になる場合があり，時間がかかる．

本文の脚注にもあるとおり，オリジナルの expmod は`xy modulo m = (x modulo m)(y modulo m) modulo m`によって大きな数値を扱わなくて済むようになっている．
