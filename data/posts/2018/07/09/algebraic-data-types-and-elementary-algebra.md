---
keywords:
  - Algebraic data types
  - Haskell
---

# 代数的データ型と初等代数学

「関数プログラミングとはなんですか？」と問われたときには「デ，データファースト……(震え声)」と答えることが多いのだが，実際HaskellやOCamlなどの言語を特徴付けるものとして，**代数的データ型**（_Algebraic Data Type_; ADT）の存在は無視できないだろう．その有用性ゆえに，近年では新たな言語の策定の際にその概念が輸出され，RustやSwiftなどの言語にも採用されている．

「代数的データ型とはなんですか？」と問われたときには—問われたことがないのでわからないのだが—おもむろにghciかutopを立ち上げて，解説を始めるのではないかと思う．ひとしきり解説をした後，「つまり直積の直和なんですよ〜🙌✨」と言って話を締めくくるだろう．

`int`型や`float`型など，「メモリ上の表現」という計算機の気持ちに極めて寄り添ったプリミティヴなデータ型や，オブジェクトがヒープに展開された先のアドレスを保持する参照型にしか馴染みがないプログラマにとっては，データ型の定義に「代数」などという仰々しい概念が登場するのは不思議に思われるかもしれない．しかし，代数的データ型自体の有用性は，少しプログラムを書けばわかる[^1]はずであるし，そもそも「代数」などという難しい言い方をしなければ，自然数どうしの足し算や掛け算，指数の計算などは，小中学校での教育を通じて万人が既に知っており，日頃から親しんでいるはずである．この記事では，人々がよく知る計算の上に成り立つ法則と，代数的データ型との類似性を示すことで，代数的データ型に親しみを持ってもらうことを目的としている．この記事を読み終えた頃には，「`Option[T]`とか`T?`とかいう型は$T+1$とでも書かれるべきものなのか〜」という理解がなされることが期待される．

基礎的なHaskellの読み書きと，概ね中学校から高校1年生くらいで習う程度の数学の知識があれば読み進められるはずである．また，この記事では再帰的に定義されたデータ型は扱わない[^2]．

---

## $1$

次のようなデータ型を考える．

```haskell
data One = One
```

これはただ1つのコンストラクタを持つ型なので，$1$と呼ぶことにする．

ところで，この型は`()`と[同型](https://kseo.github.io/posts/2016-12-25-type-isomorphism.html)である．実際，次のように関数`from :: () -> One`と`to :: One -> ()`を定義すると，`from . to == id`かつ`to . from == id`を満たすことがわかる．

```haskell
from :: () -> One
from () = One

to :: One -> ()
to One = ()
```

`One`や`()`と似たような型はいくらでも作り出すことができるが，そのうちどれを選んでも今後の議論に支障はない．以下では，2つの型$a$と$b$の間に全単射が構成できる場合，$a \simeq b$と書いてそれらを積極的に同じものとみなすことにする．ゆえに，`One`や`()`と同型である型はすべて$1$と呼ぶ．また，今後$1$を表すHaskellの型の代表として`()`型を用いる[^3]．

## $2$

次のようなデータ構造を考える．

```haskell
data Two = Head | Tail
```

これはコンストラクタを2つ持つ型なので，$2$と呼ぶことにする．

この型は`Bool`と同型である．実際，次のように関数`from :: Bool -> Two`と`to :: Two -> Bool`を定義すれば，全単射が存在することがわかる．

```haskell
from :: Bool -> Two
from False = Head
from True = Tail

to :: Two -> Bool
to Head = False
to Tail = True
```

前節で，同型な型はすべて同じものとみなすことにしたので，`Two`や`Bool`とまとめて$2$と呼ぶ．今後$2$を表すHaskellの型の代表として`Bool`を用いる．

## $3, 4, 5, \ldots$

同様に$3$, $4$, $5$ を考えることができる．

## $0$

コンストラクタをまったく持たないデータ型を考えることもできる．このような型は0個のコンストラクタを持つので$0$と呼ぶ．

```haskell
{-# LANGUAGE EmptyDataDecls #-}

data Zero
```

実は，同じような型[`Void`が`Data.Void`に既で定義されている](http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Void.html#t:Void)．

```haskell
{-# LANGUAGE EmptyCase #-}

from :: Void -> Zero
from x = case x of {}

to :: Zero -> Void
to x = case x of {}
```

とおけば，これらが同型であることがわかるので，まとめて$0$と呼ぶ．今後$0$の代表的なHaskellの型として`Void`を用いる．

## 積（乗法）

複数の型を組み合わせて，新しい型を作り出すこともできる．型引数を2つ取る以下のような型`Product`を考えてみよう．

```haskell
data Product a b = Product a b
```

`a`型が$m$個，`b`型が$n$個の値を持つとき，`Product a b`型は$m \times n$個の値を持つ．このようなアナロジーを根拠に，`a`型の値と`b`型の値を同時に持ち合わせる型を`a`と`b`の**積**と呼ぶことにする．また，`Product a b`と同型な型を総称して$a \times b$と書くことにする．

自然数どうしの掛け算であれば，掛ける順番を適当に入れ替えても答えは一致するのだった．つまり，$\times : \mathbb{N} \times \mathbb{N} \to \mathbb{N}$は以下の性質を満たす．

- 交換法則 $\forall a, b \in \mathbb{N}, a \times b = b \times a$
- 結合法則 $\forall a, b, c \in \mathbb{N}, (a \times b) \times c = a \times (b \times c)$

先ほど定義した`Product`型についても，このような性質が成り立つかを確かめてみよう．

```haskell
assocL2R :: Product (Product a b) c -> Product a (Product b c)
assocL2R (Product (Product a b) c) = Product a (Product b c)

assocR2L :: Product a (Product b c) -> Product (Product a b) c
assocR2L (Product a (Product b c)) = Product (Product a b) c

commute :: Product a b -> Product b a
commute (Product a b) = Product b a
```

結合法則が成り立つことから，$(a \times b) \times c$と$a \times (b \times c)$は同じとみなしてよいので，両者を区別せず簡単に$a \times b \times c$と書くことができる．

さて，`Product a b`は`(a, b)`と同型であるし，`Product (Product a b) c`は`(a, b, c)`と同型である．表記の簡便さのため，以降はタプルを用いることにする．

## 和（加法）

$m$個の値を持つ型`a`と$n$個の値を持つ型`b`をもとに，$m + n$個の値を持つ型を作り出すことはできるだろうか．`Either a b`はこの条件を満たす．

```haskell
data Either a b
    = Left a
    | Right b
```

積の場合と同様のアナロジーを根拠に，`Either a b`と同型な型をまとめて`a`と`b`の**和**と呼び，$a + b$と書くことする．3つ以上の和を考えたい場合，型引数とコンストラクタの数を増やせばよい．

積の場合と同様に，交換法則と結合法則も満たす．これらの確認は読者への練習問題とする．

## 簡単な計算

さて，今まで$0$やら$1$やら$2$やら，はたまた$a + b$だとか$a \times b$などという名前をいろいろな型に付けてきたが，これらの名前に妥当性はあるのだろうか．せっかくそのような名前を付けるくらいであれば，我々が知るところの計算規則—例えば$1+1=2$など—と一致していてほしいものである．ここで私が「今までのネーミングは何の根拠もなく適当に行ったので，これらの間には何らそれらしき関連性はありません」と声高に宣言することもできるが，それではわざわざこんな記事を書く意味がないので，実際にいくつかの具体的な計算を通じて確かめてみよう．

### $1+1 \simeq 2$

`()`が$1$，`Either a b`が$a+b$であったことを思い出すと，`Either () ()`は$1+1$となるが，これは$2$と同型であることが期待される．実際に計算をして確かめてみよう．

```haskell
from :: Either () () -> Bool
from (Left ()) = False
from (Right ()) = True

to :: Bool -> Either () ()
to False = Left ()
to Right = Right ()
```

### $2 \times 2 \simeq 4$

`Bool`は$2$，`(a, b)`は$a \times b$だったことを思い出すと，`(Bool, Bool)`という型は$2 \times 2$なので，これは$4$と同型であることが期待される．実際に計算を行って確かめてみよう．

```haskell
data Suit -- 4
    = Spade
    | Diamond
    | Heart
    | Club

from :: (Bool, Bool) -> Suit
from (False, False) = Spade
from (False, True) = Diamond
from (True, False) = Heart
from (True, True) = Club

to :: Suit -> (Bool, Bool)
to Spade = (False, False)
to Diamond = (False, True)
to Heart = (True, False)
to Club = (True, True)
```

### $n \times 1 \simeq n$

$1$は乗法単位元である．つまり，任意の型`n`と`()`の積は，`n`と同型である．

```haskell
from :: (n, ()) -> n
from = fst

to :: n -> (n, ())
to x = (x, ())
```

### $n + 0 \simeq n$

$0$は加法単位元である．つまり，任意の型`n`と`Void`の和は，`n`と同型である．

```haskell
import Data.Void (absurd, Void)

from :: Either n Void -> n
from (Left n) = n
from (Right x) = absurd x

to :: n -> Either n Void
to = Left
```

## 分配法則

和と積があるので，これらが分配法則$k \times (a + b) \simeq k \times a + k \times b$を満たすことを確かめてみよう．

| 数学                      | プログラム             |
| ------------------------- | ---------------------- |
| $k \times (a + b)$        | `(k, Either a b)`      |
| $k \times a + k \times b$ | `Either (k, a) (k, b)` |

という対応関係があるので，`(k, Either a b)`と`Either (k, a) (k, b)`の間に全単射を構成する．

```haskell
from :: (k, Either a b) -> Either (k, a) (k, b)
from (k, Left a) = Left (k, a)
from (k, Right b) = Right (k, b)

to :: Either (k, a) (k, b) -> (k, Either a b)
to (Left (k, a)) = (k, Left a)
to (Right (k, b)) = (k, Right b)
```

## 冪（指数）

ここからは代数的データ型の話ではなくなってしまうが，関数についても同じような法則が成り立つことを確かめよう．`a -> b`という型を持つ関数を考える．`a`型が$m$個の値を，`b`型が$n$個の値をもつ場合にこういった関数を定義しようとすると，$m$個の`a`の要素それぞれについて，どの`b`の要素に移すかを$n$通り考えることができる．つまり，$n$個の要素からひとつを選ぶ選択が$m$回生じるので，$n^m$通りのパターンが考えられる．このようなアナロジーを根拠に，`a -> b`という型を$b^a$と書くことにする．

以下の指数法則が成り立つことを確認する．

- a) $ a^ma^n=a^{m+n}$
- b) $ {(a^m)}^n = a^{mn} $
- c) $ (ab)^n = a^nb^n $

### a) $a^ma^n=a^{m+n}$

| 数学      | プログラム         |
| --------- | ------------------ |
| $a^ma^n$  | `(m -> a, n -> a)` |
| $a^{m+n}$ | `Either m n -> a`  |

という対応関係があるので，`(m -> a, n -> a)`と`Either m n -> a`の間に全単射を構成する．

```haskell
from :: (m -> a, n -> a) -> (Either m n -> a)
from (f, _) (Left x) = f x
from (_, g) (Right x) = g x

to :: (Either m n -> a) -> (m -> a, n -> a)
to f = (f . Left, f . Right)
```

### b) ${(a^m)}^n = a^{mn}$

| 数学      | プログラム      |
| --------- | --------------- |
| ${a^m}^n$ | `n -> (m -> a)` |
| $a^{mn}$  | `(m, n) -> a`   |

という対応関係があるので，`n -> (m -> a)`と`(m, n) -> a`の間に全単射を構成する．

```haskell
from :: (n -> (m -> a)) -> ((m, n) -> a)
from = uncurry . flip

to :: ((m, n) -> a) -> (n -> (m -> a))
to = flip . curry
```

この法則はカリー化に対応していることがわかった．

### c) $(ab)^n = a^nb^n$

| 数学     | プログラム         |
| -------- | ------------------ |
| $(ab)^n$ | `n -> (a, b)`      |
| $a^nb^n$ | `(n -> a, n -> b)` |

という対応関係があるので，`n -> (a, b)`と`(n -> a, n -> b)`の間に全単射を構成する．

```haskell
from :: (n -> (a, b)) -> (n -> a, n -> b)
from f = (fst . f, snd . f)

to :: (n -> a, n -> b) -> (n -> (a, b))
to (f, g) x = (f x, g x)
```

これは$\langle f, g \rangle$と書かれがちなやつ．

## まとめ

| 数学         | プログラム (Haskell) |
| ------------ | -------------------- |
| $0$          | `Void`               |
| $1$          | `()`                 |
| $2$          | `Bool`               |
| $a+b$        | `Either a b`         |
| 特に $a+1$   | `Maybe a`            |
| $a \times b$ | `(a, b)`             |
| $b^a$        | `a -> b`             |

という対応関係を考えると，よくわからないがデータ型がそれなりに代数っぽく振る舞うことがわかった．普段慣れ親しんでいる計算法則との類似性と体感して，代数的データ型に対する不安は払拭されただろうか．この記事を通じて関数プログラミングに対する興味が深まれば幸いである．

## Further reading

- [積 (圏論) - Wikipedia](<https://ja.wikipedia.org/wiki/%e7%a9%8d_(%e5%9c%8f%e8%ab%96)>)
- [余積 - Wikipedia](https://ja.wikipedia.org/wiki/%E4%BD%99%E7%A9%8D)
- [冪対象 - Wikipedia](https://ja.wikipedia.org/wiki/%E5%86%AA%E5%AF%BE%E8%B1%A1)

## 脚注

[^1]: 効果には個人差があります．
[^2]: 中高生は不動点を知らないので．
[^3]: この選出は別に恣意的なものではなく，組み込みの型を用いれば必要な準備が少なくて済むからである．
