---
keywords:
  - Haskell
  - TypeFamilyDependencies
---

# TypeFamilyDependencies の実用的な例を考える

`FunctionalDependencies` という GHC 言語拡張がある．[Haskell Wiki によると](https://wiki.haskell.org/Functional_dependencies)，

> Functional dependencies are used to constrain the parameters of type classes.

と書かれているが，これはどういうことか．

Haskell Language Report で定められた範囲では，型クラスに与えられるパラメータは1つに限られるが，[`MultiParamTypeClasses`](https://wiki.haskell.org/Multi-parameter_type_class) を用いると，複数のパラメータを与えることができる．この際に，パラメータとして与えられた (複数の) 型の間の関係性に制限を加えることができるのが，`FunctionalDependencies` なのであった．恐らく多くの人が初めて目にするのは，[`mtl` package の `MonadReader` の定義](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader-Class.html#t:MonadReader)なのではないだろうか．`| m -> r` というのがそれである．

```haskell
class Monad m => MonadReader r m | m -> r where
   ...
```

さて，GHC 8 から `TypeFamilyDependencies` という GHC 言語拡張が追加された．これについては既に lotz 先生が『型族が単射だと嬉しい理由』という記事を書いていらっしゃるのだが，(氏には失礼ながら) 少しばかりわざとらしい例だと感じたので，もう少し実務的な例を引き合いに出して，有用性を示したいと思う．

https://qiita.com/lotz/items/6c038698c8f04f57113a

---

この記事では，以下の GHC 言語拡張を使う．また，GHC のヴァージョンは 8.2.2 である．

- `AllowAmbiguousTypes`
- `OverloadedStrings`
- `ScopedTypeVariables`
- `TypeApplications`
- `TypeFamilies`
- `TypeFamilyDependencies`

以下のような `User` 型と `UserKey` 型を関係データベースで扱いたいとしよう．`UserKey` 型は (典型的には auto increment な) primary key だと思ってもらえば良い．

```haskell
data User
    = User
    { userName :: String
    , userAge  :: Int
    }

newtype UserKey = UserKey { unUserKey :: Int } deriving (Eq, Show)
```

さてここで，DBの中では primary key として `INT` 型を用いたいが，エンドユーザからはその整数の表現を隠蔽したいものとしよう．そのためには，整数をいい感じにエンコード・デコードしたものを，primary key の表現として用いればよさそうだ．このようなモチベーションはよくあるので，Hashids[^1] というライブラリが公開されている．これは様々な言語向けに実装が提供されていて，[Haskell 版も Hackage から入手可能である](https://github.com/laserpants/hashids-haskell)．

http://hashids.org/

このライブラリが (DB 上での内部表現である) `Int` と (エンドユーザが目にする表現である) `ByteString` の間の相互変換を提供してくれる．エンコードされた `UserKey` を表現する次のような型を定義しよう．

```haskell
newtype EncodedUserKey = EncodedUserKey { unEncodedUserKey :: ByteString } deriving (Eq, Show)
```

次に，`UserKey` と `EncodedUserKey` を相互に変換する `encodeUserKey` 関数と `decodeUserKey` 関数を定義する．

```haskell
encodeUserKey :: UserKey -> EncodedUserKey
encodeUserKey (UserKey n) = EncodedUserKey $ encodeUsingSalt "this is my salt" n

decodeUserKey :: EncodedUserKey -> Maybe UserKey
decodeUserKey (EncodedUserKey x) =
    case decodeUsingSalt "this is my salt" x of
        [n] -> Just (UserKey n)
        _   -> Nothing
```

GHCi で挙動を確認してみよう．

```haskell
> encodeUserKey $ UserKey 42
EncodedUserKey {unEncodedUserKey = "eP"}

> decodeUserKey $ encodeUserKey $ UserKey 42
Just (UserKey {unUserKey = 42})
```

さてここで `User` に加えて，新たに `Team` という概念が増えたとしよう．`Team` の primary key も `User` と同様に，hashids を使って隠蔽したいとする．

```haskell
data Team
    = Team
    { teamName  :: String
    , teamUsers :: [User]
    } deriving (Show)

newtype TeamKey = TeamKey { unTeamKey :: Int } deriving (Eq, Show)

newtype EncodedTeamKey = EncodedTeamKey { unEncodedTeamKey :: ByteString } deriving (Eq, Show)

encodeTeamKey :: TeamKey -> EncodedTeamKey
encodeTeamKey (TeamKey n) = EncodedTeamKey $ encodeUsingSalt "another salt" n

decodeTeamKey :: EncodedTeamKey -> Maybe TeamKey
decodeTeamKey (EncodedTeamKey x) =
    case decodeUsingSalt "another salt" x of
        [n] -> Just (TeamKey n)
        _   -> Nothing
```

`User` の場合とまったく同じ実装になってしまったので，これらを型クラスで抽象化しよう．先に `User` 型や `Team` 型を取って，その key を返す型レヴェル関数 `Key` を type family を用いて定義する．つまり，

- `User` $\mapsto$ `UserKey`
- `Team` $\mapsto$ `TeamKey`

なる型レヴェル関数である．また，ついでなので，`Key` の中身の `Int` を取り出したり，また `Int` を取って `Key` を作る部分を抽象化しておく．

```haskell
-- 何かしらの key を持つことを表す型クラス
class HasKey a where
    type Key a -- a をとって key を返す型レヴェル関数 (e.g. Key User = UserKey)
    wrapKey   :: Int -> Key a
    unwrapKey :: Key a -> Int

instance HasKey User where
    type Key User = UserKey
    wrapKey = UserKey
    unwrapKey = unUserKey

instance HasKey Team where
    type Key Team = TeamKey
    wrapKey = TeamKey
    unwrapKey = unTeamKey
```

今しがた定義した `HasKey` を前提に，いよいよ `HasCodableKey` 型クラスを定義する．

```haskell
-- Key を hashids でエンコード・デコードできることを表す型クラス
class HasKey a => HasCodableKey a where
    -- エンコードされた key (e.g. EncodedUserKey)
    type EncodedKey a

    wrapEncodedKey   :: ByteString -> EncodedKey a
    unwrapEncodedKey :: EncodedKey a -> ByteString

    -- salt は変えられるようにしておく
    salt :: ByteString

    encodeKey :: Key a -> EncodedKey a
    encodeKey key =
        let n = unwrapKey key
            bs = encodeUsingSalt (salt @a) n
        in wrapEncodedKey bs

    decodeKey :: EncodedKey a -> Maybe (Key a)
    decodeKey encodedKey =
        let bs = unwrapEncodedKey encodedKey
            ns = decodeUsingSalt (salt @a) bs
        in case ns of
            [n] -> Just (wrapKey n)
            _   -> Nothing
```

上のように定義すると，メッチャ怒られる．

```haskell
/Users/ryota-ka/dev/tf-deps-example/src/Lib.hs:64:27: error:
    • Couldn't match expected type ‘Key a0’ with actual type ‘Key a’
      NB: ‘Key’ is a type function, and may not be injective
      The type variable ‘a0’ is ambiguous
    • In the first argument of ‘unwrapKey’, namely ‘key’
      In the expression: unwrapKey key
      In an equation for ‘n’: n = unwrapKey key
    • Relevant bindings include
        key :: Key a (bound at src/Lib.hs:63:15)
        encodeKey :: Key a -> EncodedKey a (bound at src/Lib.hs:63:5)
   |
64 |         let n = unwrapKey key
   |                           ^^^

/Users/ryota-ka/dev/tf-deps-example/src/Lib.hs:66:12: error:
    • Couldn't match expected type ‘EncodedKey a’
                  with actual type ‘EncodedKey a1’
      NB: ‘EncodedKey’ is a type function, and may not be injective
      The type variable ‘a1’ is ambiguous
    • In the expression: wrapEncodedKey bs
      In the expression:
        let
          n = unwrapKey key
          bs = encodeUsingSalt (salt @a) n
        in wrapEncodedKey bs
      In an equation for ‘encodeKey’:
          encodeKey key
            = let
                n = unwrapKey key
                bs = encodeUsingSalt (salt @a) n
              in wrapEncodedKey bs
    • Relevant bindings include
        key :: Key a (bound at src/Lib.hs:63:15)
        encodeKey :: Key a -> EncodedKey a (bound at src/Lib.hs:63:5)
   |
66 |         in wrapEncodedKey bs
   |            ^^^^^^^^^^^^^^^^^

/Users/ryota-ka/dev/tf-deps-example/src/Lib.hs:70:35: error:
    • Couldn't match expected type ‘EncodedKey a2’
                  with actual type ‘EncodedKey a’
      NB: ‘EncodedKey’ is a type function, and may not be injective
      The type variable ‘a2’ is ambiguous
    • In the first argument of ‘unwrapEncodedKey’, namely ‘encodedKey’
      In the expression: unwrapEncodedKey encodedKey
      In an equation for ‘bs’: bs = unwrapEncodedKey encodedKey
    • Relevant bindings include
        encodedKey :: EncodedKey a (bound at src/Lib.hs:69:15)
        decodeKey :: EncodedKey a -> Maybe (Key a)
          (bound at src/Lib.hs:69:5)
   |
70 |         let bs = unwrapEncodedKey encodedKey
   |                                   ^^^^^^^^^^

/Users/ryota-ka/dev/tf-deps-example/src/Lib.hs:74:20: error:
    • Couldn't match type ‘Key a’ with ‘Key a3’
      Expected type: Maybe (Key a)
        Actual type: Maybe (Key a3)
      NB: ‘Key’ is a type function, and may not be injective
      The type variable ‘a3’ is ambiguous
    • In the expression: Nothing
      In a case alternative: _ -> Nothing
      In the expression:
        case ns of
          [n] -> Just (wrapKey n)
          _ -> Nothing
    • Relevant bindings include
        encodedKey :: EncodedKey a (bound at src/Lib.hs:69:15)
        decodeKey :: EncodedKey a -> Maybe (Key a)
          (bound at src/Lib.hs:69:5)
   |
74 |             _   -> Nothing
   |                    ^^^^^^^
```

よくよく読んでみると，「型レヴェル関数である `Key` とか `EncodedKey` が injective ではないぞ」と言われている．

関数 $f: A \to B$ が injective (単射) であるとは，$\forall x, y \in A$ について $x \neq y \Rightarrow f(x) \neq f(y)$ ということであるが，直感的には $f$ で写した先の集合 $B$ で要素が互いに**ぶつからない**とイメージすることができる．今回の場合 `EncodedKey` は，

- `User` $\mapsto$ `EncodedUserKey`
- `Company` $\mapsto$ `EncodedCompanyKey`

といった挙動をするが，**`User` 以外の適当な型 `a` を持ってきて，それを `EncodedUserKey` に写されると困る**のである．

関数で写した先でぶつからないということは，取りも直さず**写した先の要素 $f(x) \in B$ から，写す前の要素 $x \in A$ を一意に特定できる**ということを意味する．つまり，`EncodedUserKey` から `User` 型を特定でき，`EncodedTeamKey` からは `Team` 型を特定することができる．「この型レヴェル関数はこのように injective に振る舞いますよ，(`a` から `EncodedKey a` が定まることは当然として，逆に)`EncodedKey a` から `a` が定まることを前提に型推論してくださいね」という注記を与えるための機能こそが `TypeFamilyDependencies` だったのだ．

では実際に `TypeFamilyDependencies` を有効にして，先程のコードの型検査が通るように書き換えてみよう．

```haskell filename=before.hs
class HasKey a where
    type Key a

class HasKey a => HasCodableKey a where
    type EncodedKey a

```

```haskell filename=after.hs
class HasKey a where
    type Key a = r | r -> a

class HasKey a => HasCodableKey a where
    type EncodedKey a = r | r -> a
```

ここで `r` なり何なり適当な名前を与えてあげないと，単射性の制約が書けない[^2]．`r -> a` の部分は，型 `r` から型 `a` が一意に定まることを表している．

カインドの制約が書きたければ，次のようにも書くことができる．

```haskell
class HasKey a where
    type Key a = (r :: *) | r -> a
```

これで無事に `HasCodableKey` 型クラスが定義できたので，`User` と `Team` をこいつのインスタンスにしてやって，期待通りの動作をすることを確認しておこう．

```haskell
instance HasCodableKey User where
    type EncodedKey User = EncodedUserKey
    wrapEncodedKey = EncodedUserKey
    unwrapEncodedKey = unEncodedUserKey
    salt = "this is my salt"

instance HasCodableKey Team where
    type EncodedKey Team = EncodedTeamKey
    wrapEncodedKey = EncodedTeamKey
    unwrapEncodedKey = unEncodedTeamKey
    salt = "another salt"
```

```haskell
> encodeKey $ UserKey 42
EncodedUserKey {unEncodedUserKey = "eP"}

> decodeKey $ encodeKey $ UserKey 42
Just (UserKey {unUserKey = 42})

> encodeKey $ TeamKey 42
EncodedTeamKey {unEncodedTeamKey = "5Q"}

> decodeKey $ encodeKey $ TeamKey 42
Just (TeamKey {unTeamKey = 42})
```

今回のコードの全文は以下のとおりである．

```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Lib where

import Data.ByteString (ByteString)
import Web.Hashids (encodeUsingSalt, decodeUsingSalt)

data User
    = User
    { userName :: String
    , userAge  :: Int
    }

newtype UserKey = UserKey { unUserKey :: Int } deriving (Eq, Show)

data Team
    = Team
    { teamName  :: String
    , teamUsers :: [User]
    }

newtype TeamKey = TeamKey { unTeamKey :: Int } deriving (Eq, Show)

-- 何かしらの key を持つことを表す型クラス
class HasKey a where
    type Key a = (r :: *) | r -> a -- a をとって key を返す型レヴェル関数 (e.g. Key User = UserKey)
    wrapKey   :: Int -> Key a
    unwrapKey :: Key a -> Int

instance HasKey User where
    type Key User = UserKey
    wrapKey = UserKey
    unwrapKey = unUserKey

instance HasKey Team where
    type Key Team = TeamKey
    wrapKey = TeamKey
    unwrapKey = unTeamKey

newtype EncodedUserKey = EncodedUserKey { unEncodedUserKey :: ByteString } deriving (Eq, Show)

newtype EncodedTeamKey = EncodedTeamKey { unEncodedTeamKey :: ByteString } deriving (Eq, Show)

-- Key を hashids でエンコード・デコードできることを表す型クラス
class HasKey a => HasCodableKey a where
    -- エンコードした後のkey (e.g. EncodedUserKey)
    type EncodedKey a = (r :: *) | r -> a

    wrapEncodedKey   :: ByteString -> EncodedKey a
    unwrapEncodedKey :: EncodedKey a -> ByteString

    -- salt は変えられるようにしておく
    salt :: ByteString

    encodeKey :: Key a -> EncodedKey a
    encodeKey key =
        let n = unwrapKey key
            bs = encodeUsingSalt (salt @a) n
        in wrapEncodedKey bs

    decodeKey :: EncodedKey a -> Maybe (Key a)
    decodeKey encodedKey =
        let bs = unwrapEncodedKey encodedKey
            ns = decodeUsingSalt (salt @a) bs
        in case ns of
            [n] -> Just (wrapKey n)
            _   -> Nothing

instance HasCodableKey User where
    type EncodedKey User = EncodedUserKey
    wrapEncodedKey = EncodedUserKey
    unwrapEncodedKey = unEncodedUserKey
    salt = "this is my salt"

instance HasCodableKey Team where
    type EncodedKey Team = EncodedTeamKey
    wrapEncodedKey = EncodedTeamKey
    unwrapEncodedKey = unEncodedTeamKey
    salt = "another salt"
```

## 脚注

[^1]: FAQ にも書いているとおり，デコードができるので決してハッシュアルゴリズムを用いているわけではないが，googlability のために "hash" という語を選んでいるそうだ．
[^2]: ここの記法は少し調べるのに苦労した部分だった．Microsoft の論文などに当たってみると，associated type は open type families の場合の特殊な例であるから，同じ記法を使うし議論を省略する，といった内容が書かれていた．
