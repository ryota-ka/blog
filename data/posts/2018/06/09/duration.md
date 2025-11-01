---
keywords:
  - Haskell
  - Template Haskell
---

# Template Haskellでいい感じに時間の長さを書けるライブラリdurationを作った

Template Haskellを使って，時間の長さをいい感じに（人間が読みやすい形で）記述できるライブラリを作った．

https://hackage.haskell.org/package/duration

[](https://github.com/ryota-ka/duration)

---

## モチベーション

JavaScriptのライブラリに，msというものがある．

https://www.npmjs.com/package/ms

READMEを読めば一目で使い方が把握できるが，以下のなことができる．

```javascript
ms('2 days'); // 172800000
```

ソースコード中に`1000 * 60 * 60 * 24 * 2`などと記述するのも悪くはないが，やはり自然言語表現の方が読み手にとって親切だ．

ただし，この方法には一つ問題があって，文字列はパーズに失敗しうる．

```javascript
> const ms = require('ms')
undefined
> ms('1d')
86400000
> ms('foo')
undefined
```

パーズできない文字列を引数として与えると，`undefined`が返ってくる．実行時まで評価されない以上，避けられない問題ではあるが，明らかにバグの温床となりうる．

## コンパイル時に検証する

実行時までパーズの成否がわからないのであれば，コンパイル時にそれをやってしまえばよい．この解決策は，動的に組み立てられた文字列を引数として受け取る能力を捨ててしまうことになるが，ほとんどのユースケースでは，リテラルが与えられるであろうという想定に基づき，今回は動的な呼び出しはサポートしないことにする．Template Haskellの準クォートを使えば，任意の文字列を受け取り，パーズした上で，Haskellの式に変換することができるので，`2 days`などの文字列表現を受け取り，コンパイルの段階で，それを数値として埋め込むことが可能になる．

準クォートについては，前回の記事を参照のこと．

https://blog.ryota-ka.me/posts/2018/02/14/type-safe-embedded-and-external-json-with-template-haskell

## ユースケースその1

以下は1秒毎に`"hey!"`という文字列を標準出力に出力するプログラムの例である．

```haskell
import Control.Concurrent (threadDelay)
import Control.Monad.Fix (fix)

main :: IO ()
main = fix $ \loop -> do
    putStrLn "hey!"
    threadDelay 1000000
    loop
```

[`threadDelay`](http://hackage.haskell.org/package/base-4.11.1.0/docs/Control-Concurrent.html#v:threadDelay)は，`Int`型の値を受け取り，指定された長さだけ現在のスレッドの実行を停止するが，この際に受け取るのはマイクロ秒である．ソースコード中に現れる`1000000`という値が1秒間という長さを表していることは必ずしも自明ではない．しかし，durationを使えば以下のように記述することができる．

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Control.Concurrent (threadDelay)
import Control.Monad.Fix (fix)
import Data.Time.Clock.Duration (µs)

main :: IO ()
main = fix $ \loop -> do
    putStrLn "hey!"
    threadDelay [µs| 1s |]
    loop
```

`[µs| 1s |]`の意味するところは，「1秒間という長さをマイクロ秒に換算した値」くらいのところである．この式自体は，[`RelativeDuration a => a`](http://hackage.haskell.org/package/duration-0.1.0.0/docs/Data-Time-Clock-Duration-Types.html#t:RelativeDuration)という型を持つが，`Int`が`RelativeDuration`のインスタンスになっているのでうまくいく．他にも`Double`や`Integer`などがインスタンスになっている．

## ユースケースその2

HTTPサーヴァの実装時に，[`Web.Cookie`](http://hackage.haskell.org/package/cookie-0.4.4/docs/Web-Cookie.html)を用いて，レスポンスに`Set-Cookie`ヘッダを含める場合を考えよう．CookieのMax-ageは，[time](https://hackage.haskell.org/package/time)packageが提供する[`DiffTime`](https://hackage.haskell.org/package/time-1.9.1/docs/Data-Time-Clock.html#t:DiffTime)型で与えることができるが，durationを使えばこれを非常に簡潔に記述することができる．

```haskell
import Data.Default (def)
import Data.Time.Clock.Duration (t)
import Web.Cookie (setCookieMaxAge)

handleRequest = do
    ...
    let cookie = def { setCookieMaxAge = Just [t| 2 weeks |] }
    ...
```

準クォート自体が表す式自体は，[`AbsoluteDuration a => a`](https://hackage.haskell.org/package/duration-0.1.0.0/docs/Data-Time-Clock-Duration-Types.html#t:AbsoluteDuration)型を持ち`DiffTime`や`NominalDiffTime`，`CUSeconds`など，単位を伴った時間の長さを表す型がインスタンスになっている．

## その他

[@hiroqn](https://github.com/hiroqn)がOCamlヴァージョンを作ったらしい．お楽しみに．

（2018/06/30追記）公開された

[](https://qiita.com/hiroqn@github/items/a676040612534fa1b0e6)
