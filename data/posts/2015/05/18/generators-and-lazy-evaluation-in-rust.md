---
keywords:
  - Lazy evaluation
  - Rust
---

# Rust でジェネレータを作ったり，遅延評価してみる

## はじめに

日本時間の2015年5月16日に Rust 1.0 がリリースされた．
というわけで，4月末頃から延々と「書こう書こう」と言っていた記事を，いい加減に書こうと思う．

## これまでの流れ

- [Ruby の Enumerator でジェネレータを作ったり，遅延評価してみる](https://blog.ryota-ka.me/posts/2015/04/23/generators-and-lazy-evaluation-in-ruby)
- [Python でジェネレータを作ったり，遅延評価してみる](https://blog.ymyzk.com/2015/04/python-generator-lazy/)
- [ECMAScript 6 でジェネレータを作ったり，遅延評価してみる](https://blog.ymyzk.com/2015/04/ecmascript-6-generator-lazy/)

---

## イテレータの例

```rust
fn main() {
    let arr = [0, 1, 2, 3];  // 要素は4つしかない
    let mut iter = arr.iter();

    for _ in 0..5 {  // ループを5回回してみる
        match iter.next() {
            Some(x) => println!("{}", x),
            None    => println!("I don't have values anymore!")
        }
    }
}
```

このコードの出力結果は以下のようになる．

```plaintext
0
1
2
3
I don't have values anymore!
```

イテレータの内部に返すべき値 `x` が残っていれば，`Some(x)` を，残っていなければ `None` を返している様子がわかる．
このように，Rust のイテレータの `next()` メソッドは一般に `Option<T>` を返す．
一方，`for ... in` は，イテレータから `None` を得た際に自動的にループを終了する．こちらも簡単な例で示しておく．

```rust
fn main() {
    let arr = [0, 1, 2, 3];
    let iter = arr.iter();
    for i in iter {  // 0 1 2 3 と順に表示する
        println!("{}", i)
    }
}
```

もちろん，上の例で示したように，`for i in 0..4 { ... }` と書くこともできる．
また，`next()` メソッドなどの，イテレータ内部の状態を変化させるメソッドを使用しない場合，変数に `mut` キーワードを付けて宣言する必要はない．

## Rust のイテレータについて

外部イテレータのためのインターフェースとして，[`std::iter::Iterator`](https://doc.rust-lang.org/std/iter/trait.Iterator.html) というトレイト[^1]が用意されており，このトレイトを実装したデータ型は，イテレータとして振る舞うことができる[^2]．
データ型に対してこのトレイトを実装する手順は以下の2つである．

- `Item` associated type でイテレータが返す値の型を宣言する
- `next()` メソッドを実装する

`next()` メソッドの型シグネチャは `fn next(&mut self) -> Option<Self::Item>` となっている．

## フィボナッチ数列を返すイテレータ

実際に，整数の二つ組を持った構造体 `Fibonacci` に対して，`Iterator` トレイトを実装してみよう．

```rust
struct Fibonacci {
    state: (u64, u64)
}

impl Iterator for Fibonacci {
    type Item = u64;

    fn next(&mut self) -> Option<u64> {
        let (a, b) = self.state;
        self.state = (b, a + b);

        Some(a)
    }
}

impl Fibonacci {
    fn new() -> Fibonacci {
        Fibonacci { state: (0, 1) }
    }
}
```

`next()` メソッドを実行する度に，イテレータが内部の状態を変えつつ，`Option<u64>` 型の値を返す様子がわかる．
また，`new()` associated function で，最初の2項である `0`, `1` を持った `Fibonacci` 構造体を作って返すようにした．

```rust
fn main() {
    let mut fib = Fib::new();

    println!("{}", fib.next().unwrap()); // 0
    println!("{}", fib.next().unwrap()); // 1
    println!("{}", fib.next().unwrap()); // 1
    println!("{}", fib.next().unwrap()); // 2
    println!("{}", fib.next().unwrap()); // 3
}
```

また，`main()` 関数の中身を以下のように書き換えてみよう．

```rust
fn main() {
    for i in Fibonacci::new()
        .map(|x| x.pow(2))
        .filter(|x| x % 2 == 1)
        .take(10)
    {
        println!("{}", i);
    }
}
```

このコードの出力結果は以下のようになる．

```plaintext
1
1
9
25
169
441
3025
7921
54289
142129
```

## 脚注

[^1]: Haskell における型クラスのようなものと考えてもらえばよい．
[^2]: 詳細は前述のドキュメントを参照のこと．
