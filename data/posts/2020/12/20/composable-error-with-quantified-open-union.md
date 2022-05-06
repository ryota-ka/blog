---
keywords:
  - Haskell
  - Open union
  - Software architecture
---

# ユースケース層が投げうるエラーの型を「量化した open union」にしておけば複数のユースケースを合成したときに上の層でエラーハンドリングが楽にできて最高です！

この記事は [Haskell Advent Calendar 2020](https://qiita.com/advent-calendar/2020/haskell) 20日目の記事です．

## TL; DR

タイトル

---

## 問題設定

ユースケース層とサーバ層が存在する Web アプリケーションを考える．サーバ層はユースケース層を呼び出すことができるが，ユースケース層はサーバ層について無知でなければならない．

```plaintext
+----------------+
|  server layer  |
+-------+--------+
        |  depends on
+-------v--------+
| use case layer |
+----------------+
```

このようなアプリケーションのAPI リクエストハンドラにおいて，複数のユースケースを合成して呼び出すケースを例に挙げ，それぞれのユースケースが投げうるエラーをうまく扱う open union を用いたテクニックを紹介する．

今回は extensible effects を用いてユースケース層を記述するが，特に extensible effects に固有の話というわけではない．tagless final でも，`ExceptT` や単なる `Either` でも同じテクニックが使えるだろう．

extensible effects のライブラリとして [`extensible-skeleton`](https://hackage.haskell.org/package/extensible-skeleton) を用いる．

## `Error` effect

ユースケース層はエラーを投げることができるものとする．下準備として，エラーを投げるための `Error` effect を定義しておく．

```haskell filename=src/Effects/Error.hs
module Effects.Error
    ( HasEff
    , throw
    ) where

import "base" Data.Proxy (Proxy (..))
import "extensible-skeleton" Data.Extensible.Effect (Eff, EitherEff, throwEff)
import "membership" Type.Membership (Lookup)

type HasEff e effs = Lookup effs "Error" (EitherEff e)

throw
    :: forall e effs void
     . HasEff e effs
    => e
    -> Eff effs void
throw e = throwEff (Proxy @"Error") e
```

## ユースケース

引数で受け取った ID に対応する `Talent` エンティティを返すユースケースである `findTalent` というユースケースがあるとしよう．ただし，対応する `Talent` が見付からなかった場合には `TalentNotFound` エラーを送出する．

```haskell filename=src/UseCases/FindTalent.hs
module UseCases.FindTalent where

import "extensible-skeleton" Data.Extensible.Effect (Eff)

import qualified Effects.Error as Error (HasEff, throw)
import qualified Effects.TalentRepository as TalentRepository (find, HasEff)

{-
与えられた 'TalentID' に対応する 'Talent' を返す
見付からない場合には 'TalentNotFound' を投げる
-}
findTalent
    :: forall effs
     . TalentRepository.HasEff effs
    => Error.HasEff TalentNotFound effs
    => TalentID
    -> Eff effs Talent
findTalent id = do
    mtalent <- TalentRepository.find id
    case mtalent of
        Nothing -> do
            let e = TalentNotFound id
            Error.throw e
        Just talent -> pure talent
```

`findTalent` とよく似た `findTag` ユースケースも準備しておく．

```haskell filename=src/UseCases/FindTag.hs
module UseCases.FindTag where

import "extensible-skeleton" Data.Extensible.Effect (Eff)

import qualified Effects.Error as Error (HasEff, throw)
import qualified Effects.TagRepository as TagRepository (find, HasEff)

{-
与えられた 'TagID' に対応する 'Tag' を返す
見付からない場合には 'TagNotFound' を投げる
-}
findTag
    :: forall effs
     . TagRepository.HasEff effs
    => Error.HasEff TagNotFound effs
    => TagID
    -> Eff effs Tag
findTag id = _ -- 実装は割愛
```

## サーバ層からユースケースを呼び出す

これら2つのユースケースを合成し，サーバ層から呼び出す場面に焦点を当てる．

ここで `Handler` は，API のリクエストハンドラを書くための言語であり，`runUseCase` は，ユースケース記述言語から `Handler` 言語への解釈を与える関数とする．ただし，`Error` effect は `Either` として解釈する．

```haskell filename=src/Server/Handlers/API/Taggings/Create.hs
module Server.Handlers.API.Taggings.Create where

import UseCases.FindTag (findTag)
import UseCases.FindTalent (findTalent)

data Response = Response { ok :: Bool }

handler :: TalentID -> TagID -> Handler Response
handler talentID tagID = do
    result <- runUseCase $ do -- この do 以下で複数のユースケースを合成する
        talent <- findTalent talentID
        tag    <- findTag tagID
        createTagging talent tag

    case result of
        Left e  -> handleErrors e
        Just () -> pure Response { ok = True }

-- この関数でユースケースのエラーハンドリングを行いたい
handleErrors
    :: _ -- が，引数の型は？
    -> Handler Response
handleErrors = _
```

しかし，これはうまくいかない．`findTalent` が投げうるエラーの型は `TalentNotFound` である一方，`findTag` が投げうるエラーの型は `TagNotFound` であり，これらが一致しないからだ．これは本質的には (`e` と `e'` が異なる型であるときに) `Either e` と `Either e'` が組み合わせられない問題と同質である．

## open union

そもそも一般に，あるユースケースが投げたいエラーが1種類で事足りるとは限らない．ユースケースによっては，あらかじめ宣言した複数の種類のエラーのうち，状況に応じてどれかひとつを投げたい，という場合もあるだろう．このような欲求を満たすため，まずは投げるエラーを open union に埋め込むことにしよう．今回は既に `extensible` package を使っているので，`extensible` が提供する extensible sum を使うことにする．

```diff filename=src/UseCases/FindTalent.hs
@@ -1,5 +1,6 @@
 module UseCases.FindTalent where

+import "extensible" Data.Extensible.Plain (bury, OneOf)
 import "extensible-skeleton" Data.Extensible.Effect (Eff)

 import qualified Effects.Error as Error (HasEff, throw)
@@ -12,7 +13,7 @@ import qualified Effects.TalentRepository as TalentRepository (find, HasEff)
 findTalent
     :: forall effs
      . TalentRepository.HasEff effs
-    => Error.HasEff TalentNotFound effs
+    => Error.HasEff (OneOf '[TalentNotFound]) effs
     => TalentID
     -> Eff effs Talent
 findTalent id = do
@@ -20,5 +21,5 @@ findTalent id = do
     case mtalent of
         Nothing -> do
             let e = TalentNotFound id
-            Error.throw e
+            Error.throw (bury e)
         Just talent -> pure talent
```

また，`findTag` ユースケースにも同様の変更を加える．

## 投げられうるエラーを量化する

エラーを open union で扱うだけでは，エラーの型が一致しないという当初の問題が解消されるわけではない． `OneOf '[TalentNotFound]` と `OneOf '[TagNotFound]` は異なるので当然である．そこで，`OneOf` の引数を「少なくとも `TalentNotFound` を含む任意のリスト」という風に量化する．

```diff filename=src/UseCases/FindTalent.hs
@@ -1,5 +1,6 @@
 module UseCases.FindTalent where

+import "extensible" Data.Extensible (Include)
 import "extensible" Data.Extensible.Plain (bury, OneOf)
 import "extensible-skeleton" Data.Extensible.Effect (Eff)

@@ -11,9 +12,10 @@ import qualified Effects.TalentRepository as TalentRepository (find, HasEff)
 見付からない場合には 'TalentNotFound' を投げる
 -}
 findTalent
-    :: forall effs
+    :: forall errors effs
      . TalentRepository.HasEff effs
-    => Error.HasEff (OneOf '[TalentNotFound]) effs
+    => errors `Include` '[TalentNotFound]
+    => Error.HasEff (OneOf errors) effs
     => TalentID
     -> Eff effs Talent
 findTalent id = do
```

新たな型変数 `errors` を導入し，エラーの型として `OneOf errors` を用いるように変更を加えた．

`findTag` が投げうるエラーについても同様に，**少なくとも `TagNotFound` を含む任意のリスト**という風に量化してやる．

こうすれば，`handleErrors` 関数は以下のように実装できる．ここにおいて，`findTalent` および `findTag` のシグネチャ中の型変数 `errors` はいずれも `'[TagNotFound, TalentNotFound]` に解決される．

```haskell
handleErrors
    :: OneOf '[TagNotFound, TalentNotFound]
    -> Handler Response
handleErrors = match
    $  Match (\(Identity (TagNotFound _)) -> throw NotFound)
    <: Match (\(Identity (TalentNotFound _)) -> throw NotFound)
    <: nil
```

要件が変更され，合成されたユースケースの最後で呼び出されていた `createTagging` が `TagAlreadyAttachedToTalent` エラーや `TooManyTagsAttachedToTalent` エラーを投げるようになったとしよう．その場合でも，パターンマッチを増やすだけで対応できるため，拡張性にも富んでいる．

```haskell
handleErrors
    :: OneOf '[TagNotFound, TalentNotFound, TagAlreadyAttachedToTalent, TooManyTagsAttachedToTalent]
    -> Handler Response
handleErrors = match
    $  Match (\(Identity (TagNotFound _)) -> throw NotFound)
    <: Match (\(Identity (TalentNotFound _)) -> throw NotFound)
    <: Match (\(Identity (TagAlreadyAttachedToTalent _)) -> pure Response { ok = True }) -- return 2xx for idempotent requests
    <: Match (\(Identity (TooManyTagsAttachedToTalent _)) -> throw Conflict)
    <: nil
```

## 広告

HERP 広告

この記事は HERP 勤務中に書かれた。

HERP は本物の Haskell プログラマーを募集しています。

https://github.com/herp-inc/engineering-careers

## コード

主要な部分のみを抜粋した不完全なコードである．GHC 言語拡張や，細かい関数・データ型の定義などは適宜補ってほしい．また，本文中で最初に定義した `Error` effect は，量化された open union をエラーとして投げることを前提した `UseCaseError` effect で置き換えている．

```haskell filename=src/Effects/UseCaseError.hs
module Effects.UseCaseError
    ( HasEff
    , throw
    ) where

import "base" Data.Proxy (Proxy (..))
import "extensible" Data.Extensible.Plain (bury, OneOf)
import "extensible-skeleton" Data.Extensible.Effect (Eff, EitherEff, throwEff)
import "membership" Type.Membership (Lookup, Member)

type HasEff errors effs = Lookup effs  "UseCaseError" (EitherEff (OneOf errors))

throw
    :: forall errors e effs void
     . Member errors e
    => HasEff errors effs
    => e
    -> Eff effs void
throw e = throwEff (Proxy @"UseCaseError") (bury e)
```

```haskell filename=src/UseCases/FindTalent.hs
module UseCases.FindTalent where

import "extensible" Data.Extensible (Include)
import "extensible-skeleton" Data.Extensible.Effect (Eff)

{-
与えられた 'TalentID' に対応する 'Talent' を返す
見付からない場合には 'TalentNotFound' を投げる
-}
findTalent
    :: forall errors effs
     . errors `Include` '[TalentNotFound]
    => TalentRepository.HasEff effs
    => UseCaseError.HasEff errors effs
    => TalentID
    -> Eff effs Talent
findTalent id = do
    mtalent <- TalentRepository.find id
    case mtalent of
        Nothing -> do
            let e = TalentNotFound id
            UseCaseError.throw e
        Just talent -> pure talent
```

```haskell filename=src/UseCases/FindTag.hs
module UseCases.FindTag where

import "extensible" Data.Extensible (Include)
import "extensible-skeleton" Data.Extensible.Effect (Eff)

{-
与えられた 'TagID' に対応する 'Tag' を返す
見付からない場合には 'TagNotFound' を投げる
-}
findTag
    :: forall errors effs
     . errors `Include` '[TagNotFound]
    => TagRepository.HasEff effs
    => UseCaseError.HasEff errors effs
    => TagID
    -> Eff effs Tag
findTag id = do
    mtag <- TagRepository.find id
    case mtag of
        Nothing -> do
            let e = TagNotFound id
            UseCaseError.throw e
        Just tag -> pure tag
```

```haskell filename=src/Server/Handlers/API/Taggings/Create.hs
module Server.Handlers.API.Taggings.Create where

import "base" Data.Functor.Identity (Identity (..))
import "extensible" Data.Extensible ((<:), Match (..), match, nil)
import "extensible" Data.Extensible.Plain (OneOf)

import UseCases.FindTag (findTag)
import UseCases.FindTalent (findTalent)

handler :: TalentID -> TagID -> Handler Response
handler talentID tagID = do
    result <- runUseCase $ do
        talent <- findTalent talentID
        tag    <- findTag tagID
        createTagging talent tag

    case result of
        Left e  -> handleErrors e
        Just () -> pure Response { ok = True }

handleErrors
    :: OneOf '[TagNotFound, TalentNotFound]
    -> Handler Response
handleErrors = match
    $  Match (\(Identity (TagNotFound _)) -> throw NotFound)
    <: Match (\(Identity (TalentNotFound _)) -> throw NotFound)
    <: nil
```
