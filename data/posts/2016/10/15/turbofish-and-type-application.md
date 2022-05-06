---
keywords:
  - Haskell
  - Rust
  - Type application
---

# Rust の turbofish と GHC 8 の Type Application ― または我々は如何にして多相な関数を単相化するか

Rust には [`std::str::FromStr`](https://doc.rust-lang.org/std/str/trait.FromStr.html) という trait があり，データ型がこれを実装すると，`from_str` という名前の [associated function](https://doc.rust-lang.org/book/method-syntax.html#associated-functions)[^1] を通じて，[`str`](https://doc.rust-lang.org/std/primitive.str.html) からそのデータ型に変換できるようになる．

---

```rust
use std::str::FromStr;

fn main() {
    let x = i32::from_str("42");
    println!("{}", x.unwrap()) // 42
}
```

これだけ見れば，特に取り立てて議論するべき点はない．

一方．`str` は [`parse`](https://doc.rust-lang.org/std/primitive.str.html#method.parse) というメソッドを持っていて，文字通り文字列のパーズを行うのだが，以下のようなシグネチャをしている．

```rust
fn parse<F>(&self) -> Result<F, F::Err>
where F: FromStr
```

`str` である自身を受け取って，`Result<F, F::Err>` 型を返す―ただし，`F` は `FromStr` trait を実装している[^2]―といったところだ．前述した ` std::str::FromStr::from_str` と同じことをしているが，いわば見る視点が逆転しているのである．つまり，`std::str::FromStr::from_str` は，`Self` から `str` を，`std::str::parse` は，`str` から `F` を，それぞれ眺めている．

さて，前者は `str` に視点を定めればよいのは明らかだが，立場が逆になるとうまくいかない．`F` は多相なので，どこを見ればよいかが定かでない．

具体例を挙げよう．次のコードはコンパイルできない．`"42"` という文字列をどの型の値としてパーズしたいかがわからないからだ．

```rust
fn main() {
    let x = "42".parse();
    println!("{}", x.unwrap())
}

/*
error[E0284]: type annotations required: cannot resolve `<_ as std::str::FromStr>::Err == _`
 --> /var/folders/5h/7wt7yl7n24v72zsz77_3w_w00000gn/T/vKY0lB7/51.rs:2:18
  |
2 |     let x = "42".parse();
  |                  ^^^^^
*/
```

エラーメッセージに従って，型注釈を与えてやれば，コンパイルに成功する．

```rust
use std::num::ParseIntError;

fn main() {
    let x: Result<i32, ParseIntError> = "42".parse();
    println!("{}", x.unwrap()) // 42
}
```

ところで，`i32` のパーズに失敗した際のエラーは `ParseIntError` だと分かり切っている．`i32` という記述から推論させられないものだろうか．

実は Rust は，多相である `parse` メソッドに，変換先の型の情報を渡して，型を限定する文法を提供している．これは "turbofish" と呼ばれ `::<>` という形をしている．先程のコードを turbofish を使って書き直す場合，`parse` メソッドとメソッド呼び出しの `()` の間に `::<i32>` と書いてやる．

```rust
fn main() {
    let x = "42".parse::<i32>();
    println!("{}", x.unwrap()) // 42
}
```

関数の型 (`&str -> Result<i32, ParseIntError>`) を直接指定しているのではなく，関数に型 `i32` を，あたかも引数のように与えているという点に注目してほしい．これは $\Lambda$ で抽象化された型 `F` に `i32` という具体型を渡して，関数全体の型を決定するという操作に相当しているのだと思う．

さて，Haskell にこのような文法はなかったかと考えたが，先日 [Haskell Day 2016](http://connpass.com/event/37892/) に赴いた際に，SPJ が [System F](https://en.wikipedia.org/wiki/System_F) の話をしていて，GHC 8.0 から，[Type Application](https://ghc.haskell.org/trac/ghc/wiki/TypeApplication) という機能が導入された[^3]と話していたことを思い出した．これを用いると，Haskell でも以下のように `@Int` という記法で，多相な関数に `Int` 型を適用して単相化することができる．

```haskell
{-# LANGUAGE TypeApplications #-}

import Text.Read (readEither)

unwrap :: Either a b -> b
unwrap = either undefined id

main = print $ unwrap (readEither @Int "42") -- 42
```

Rust の `Result` に対応して，`Either` を用いた．

GHCi を使えば，多相な関数に型を適用して，単相な関数にする過程を実際に確かめることができる．

```haskell
$ ghci

> :set -XTypeApplications

> :t read
read :: Read a => String -> a

Prelude> :t read "42"
read "42" :: Read a => a

> read "42"
*** Exception: Prelude.read: no parse

> :t read @Int
read @Int :: String -> Int

> :t read @Int "42"
read @Int "42" :: Int

> read @Int "42"
42
```

Hindley-Milner 型システムは強力であり，プログラマが直接型を明示しなければコンパイルできない，といった状況はそう多くない．しかしながら，幾つか例外があり，そのような場合に値ないし関数に，完全な形で注釈を与えることは，しばしば煩わしい作業である．`let x: Result<i32, ParseIntError>` や `(read :: String -> Int) 42` などという冗長な書き方をしなくても済むように，このような仕組みを言語や処理系が提供してくれることは心強い．

以上の内容は，rustc 1.12.0 および ghc 8.0.1 での挙動に基づいている．

## 脚注

[^1]: 引数として self を取らないもの．他の言語で言う static method や class method などに相当する．
[^2]: 個人的にこの `where` というキーワードは (少なくとも初学者にとっては) Haskell における `=>` よりもわかりやすいと思っている．Rust の影響を色濃く受ける Swift でも採用されている．
[^3]: 実際には，以前から内部的に実装されていたものを，一部使えるようにしたらしい．
