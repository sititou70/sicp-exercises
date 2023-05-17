Louis の実装では、serialized-exchange が終了しない。

serialized-exchange による直列化の内部で、Louis による make-account-and-serializer 内部での直列化が呼ばれてしまうため、処理がそれ以上進行しなくなってしまう。

内部的には同じ mutex を 2 回取得しようとするため、ビジーウェイトで止まってしまう。
