# Template Haskell でコード中に JSON を埋め込んだりコンパイル時にファイルから型安全に読み込んだりする

[前回](http://ryota-ka.hatenablog.com/entry/2018/01/25/031605)よりはもう少し実用的な例を．

Template Haskell を使って，Haskell のコード中に JSON をそのまま埋め込むことができるようにする．また，あらかじめ用意しておいた JSON ファイルをコンパイル時に読み込み，指定したデータ型の値にする．

## ToC

1. コード中に JSON を埋め込む
2. コンパイル時に JSON をファイルから型安全に読み込む

---

## 環境

```sh
stack --version
Version 1.6.3 x86_64 hpack-0.20.0

$ stack ghc -- --version
The Glorious Glasgow Haskell Compilation System, version 8.2.2

$ stack list-dependencies | grep -e aeson -e template-haskell
aeson 1.2.4.0
template-haskell 2.12.0.0
```

## 1. コード中に JSON を埋め込む

準クォート (quasi quote) を使う．

前回説明した通り，クォートには以下の4種類があるのだった．

- `[e| ... |]`
  - **e**xpression
  - `Q Exp` 型を持つ
- `[d| ... |]`
  - **d**eclarations
  - `Q [Dec]` 型を持つ
- `[t| ... |]`
  - **t**ype
  - `Q Type` 型を持つ
- `[p| ... |`
  - **p**attern
  - `Q Pat` 型を持つ

では**準**クォートとは何だろうか．[GHC User's Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#th-quasiquotation) によると，以下のように説明されている(一部抜粋)．

- 準クォートは `[quoter| string |]` という形をしている
- `quoter` はインポートされた _quoter_ の名前である
- `string` は任意の文字列である
- `quoter` は [`Language.Haskell.TH.Quote.QuasiQuoter`] 型の値である

実際に，[`Language.Haskell.TH.Quote`](https://hackage.haskell.org/package/template-haskell-2.12.0.0/docs/Language-Haskell-TH-Quote.html) 内で宣言されている [`QuasiQuoter`](https://hackage.haskell.org/package/template-haskell-2.12.0.0/docs/Language-Haskell-TH-Quote.html#t:QuasiQuoter) 型の定義を確認してみよう．

```haskell
data QuasiQuoter
    = QuasiQuoter
    { quoteExp  :: String -> Q Exp   -- 式
    , quotePat  :: String -> Q Pat   -- パターン
    , quoteType :: String -> Q Type  -- 型
    , quoteDec  :: String -> Q [Dec] -- 宣言
    }
```

文字列を受け取って，式・パターン・型・トップレベル宣言にそれぞれ変換するものであると主張している．`[quoter| string |]` の `string` の部分が**任意の**文字列だったことを思い出すと，準クォートを通じてできることは，任意の文字列を受け取り，それをパーズし，Haskell のプログラムに変換することであることがわかる．つまり，パーザさえ書けば Haskell のコード中に任意の言語を埋め込めてしまう！

さて，準クォートとはなんぞやということがわかったところで，実際に quasiquoter を定義し，JSON をコード中に埋め込んでみよう．ここで JSON のパーザを用意しないといけないのだが，今回の目的は JSON のパーザを書くことではないし，自前で実装したところで performant であるとは思えないので，素直に [`aeson`](https://hackage.haskell.org/package/aeson) のパーザを使うことにする．

適当に `stack new` でプロジェクトを生成し，`package.yaml` の `dependeicies` に `aeson` と `bytestring` と `template-haskell` を追加する．

コードはこんな感じで．

```haskell
-- src/Lib.hs

{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( json
    ) where

import Data.Aeson (eitherDecode, Value)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter, quoteExp))

import qualified Data.ByteString.Lazy.Char8 as C8

json :: QuasiQuoter
json = QuasiQuoter { quoteExp = buildJSONExp . parseExp }

parseExp :: String -> Value
parseExp str =
    let result = eitherDecode (C8.pack str) in
    case result of
        Left err -> error err
        Right json -> json

buildJSONExp :: Value -> Q Exp
buildJSONExp value = [e| value |]
```

```haskell
-- app/Main.hs

{-# LANGUAGE QuasiQuotes #-}

module Main where

import Lib (json)

main :: IO ()
main = print
    [json|
        {
            "name": "ryota-ka",
            "age": 24,
            "spouse": null
        }
    |]
```

実行してみる．

```sh
$ stack build && stack exec -- json-th-exe
Object (fromList [("spouse",Null),("age",Number 24.0),("name",String "ryota-ka")])
```

`Data.Aeson.Value` 型の値が得られている．malformed な JSON を渡すと，きちんとコンパイル時にエラーになるはず[^1]である．

`QuasiQuoter` の4つのフィールドのうち `quoteExp` しか初期化していないが，[Hackage にも以下のように書かれている](https://hackage.haskell.org/package/template-haskell-2.12.0.0/docs/Language-Haskell-TH-Quote.html#t:QuasiQuoter)．

> if you are only interested in defining a quasiquoter to be used for expressions, you would define a `QuasiQuoter` with only `quoteExp`, and leave the other fields stubbed out with errors.

## 2. コンパイル時に JSON をファイルから型安全に読み込む

コード中に JSON をそのままベタ書きするのはあまり格好良くないので，JSON ファイルを用意して，コンパイル時に読み込むことにする．またその際に，`Value` 型ではなく，`FromJSON` のインスタンスである任意の型として読み込み，パーズできなかった場合にはコンパイル時にエラーを吐くようにしたい．

まずは JSON から読み込みたいデータ型を定義する．

```haskell
-- src/Person.hs

{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}

module Person
    ( Person (..)
    ) where

import Data.Aeson ((.:), FromJSON (parseJSON), withObject)
import Language.Haskell.TH.Syntax (Lift)

data Person
    = Person
    { name   :: String
    , age    :: Int
    , spouse :: Maybe Person
    } deriving (Lift, Show)

instance FromJSON Person where
    parseJSON = withObject "Person" $ \o -> Person
        <$> o .: "name"
        <*> o .: "age"
        <*> o .: "spouse"
```

「`Lift` ってなんやねん」と思ってしまうが，[焦らずに Hackage を確認すると](https://hackage.haskell.org/package/template-haskell-2.12.0.0/docs/Language-Haskell-TH-Syntax.html#t:Lift)，トップレベルにない Oxford brackets (`[| ... |]`) に式を埋め込む際に必要らしい．`DeriveLift` を有効化することでインスタンスの自動導出を行える．

次に，`src/Lib.hs` に以下のように書き加える[^2]．

```diff
diff --git a/src/Lib.hs b/src/Lib.hs
index 473663e..1e6caba 100644
--- a/src/Lib.hs
+++ b/src/Lib.hs
@@ -1,20 +1,31 @@
+{-# LANGUAGE AllowAmbiguousTypes #-}
 {-# LANGUAGE TemplateHaskell #-}
+{-# LANGUAGE ScopedTypeVariables #-}
+{-# LANGUAGE TypeApplications #-}

 module Lib
     ( json
+    , loadJSONFile
     ) where

 import Data.Aeson
     ( eitherDecode
+    , FromJSON
+    , fromJSON
+    , Result (Error, Success)
     , Value
     )
 import Language.Haskell.TH
     ( Exp
     , Q
+    , runIO
     )
 import Language.Haskell.TH.Quote
     ( QuasiQuoter (QuasiQuoter, quoteExp)
     )
+import Language.Haskell.TH.Syntax
+    ( Lift
+    )

 import qualified Data.ByteString.Lazy.Char8 as C8

@@ -30,3 +41,12 @@ parseExp str =

 buildJSONExp :: Value -> Q Exp
 buildJSONExp value = [e| value |]
+
+loadJSONFile :: forall a. (FromJSON a, Lift a) => FilePath -> Q Exp
+loadJSONFile filename = do
+    str <- runIO $ readFile filename
+    let json = parseExp str
+    case fromJSON @a json of
+        Success x -> [e| x |]
+        Error err -> error err
```

`loadJSONFile` の部分だけ抜き出すと，

```haskell
loadJSONFile :: forall a. (FromJSON a, Lift a) => FilePath -> Q Exp
loadJSONFile filename = do
    str <- runIO $ readFile filename
    let json = parseExp str
    case fromJSON @a json of
        Success x -> [e| x |]
        Error err -> error err
```

`runIO :: IO a -> Q a` がミソで，コンパイル時に任意の IO 処理を実行して，`Q` モナドに変換することができる．強い．

最後に `app/Main.hs` を以下のように変更する．

```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Lib (json, loadJSONFile)
import Person (Person)

person :: Person
person = $(loadJSONFile @Person "person.json")

main :: IO ()
main = print person
```

プロジェクトのルートディレクトリに `person.json` を用意してビルド・実行すると以下の通り．

```sh
$ stack build && stack exec -- json-th-exe
Person {name = "ryota-ka", age = 24, spouse = Nothing}
```

`person.json` の中身の JSON を，`Person` 型としてパーズできないように，例えば `age` キーを消すなどしてやって，再度ビルドを実行すると，コンパイル時にエラーになってくれるはず[^3]だ．

## ソースコード

[](https://github.com/ryota-ka/json-th)

## 脚注

[^1]: 疑り深い読者の方は実際にお試しあれ！
[^2]: `parseExp` 関数を使い回しているが，かつては Oxford brackets の中の expression を parse する関数だったのが，今ではファイルから得られた文字列のパーズに使っているので，名前として不適な感はある．
[^3]: こちらも疑り深い読者の方は実際にお試しあれ！
