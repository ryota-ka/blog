# Template Haskell でコンパイル時 FizzBuzz

数ヶ月前に Twitter で，コンパイル時に FizzBuzz を計算して，実行時には計算された文字列を出力をするだけ，というコンパイル時 FizzBuzz を何かの言語でやっているのを見かけた．元ネタは江添さんがC++で書いたものらしい．インスピレーションを受けて，Haskell で書いてはみたが，簡単すぎて全然おもしろくなくなってしまった．

---

## コード

GHC の制約[^1]により，モジュールを分割している．

```haskell filename=FizzBuzz.hs
{-# LANGUAGE TemplateHaskell #-}

module FizzBuzz (answerExpr) where

import Language.Haskell.TH

fizzbuzz :: Int -> String
fizzbuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3 == 0 = "Fizz"
  | n `mod` 5 == 0 = "Buzz"
  | otherwise = show n

answer :: String
answer = unlines . map fizzbuzz $ [1..100]

answerExpr :: Q Exp
answerExpr = pure . LitE . StringL $ answer
```

```haskell filename=Main.hs
{-# LANGUAGE TemplateHaskell #-}

module Main where

import FizzBuzz (answerExpr)

main :: IO ()
main = putStrLn $(answerExpr)
```

## 解説

ランタイムで計算を行うのではなく，出力される文字列をコンパイル時にあらかじめ計算させておくことを目指す．最終的な成果物は，以下のプログラムと同等になる．

```haskell
main :: IO ()
main = putStrLn "1\n2\nFizz\n4\nBuzz\nFizz\n..." -- 以下略
```

なので，`"1\n2\nFizz\n4\nBuzz\nFizz\n..."` という値のリテラルを表す構文木をコンパイル時に計算しておいて，接合してやればよさそうだ．[GHC の User's Guide によると](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#th-syntax)

> A splice can occur in place of
>
> - an expression; the spliced expression must have type `Q Exp`
> - a pattern; the spliced pattern must have type `Q Pat`
> - a type; the spliced expression must have type `Q Type`
> - a list of declarations at top level; the spliced expression must have type `Q [Dec]`

今回欲しいのは expression なので，`Q Exp` 型の値を作ることを目的とする．

また，

> A expression quotation is written in Oxford brackets, thus:
>
> - `[| ... |]`, or `[e| ... |]`, where the ”...” is an expression; the quotation has type `Q Exp`.
> - `[d| ... |]`, where the ”...” is a list of top-level declarations; the quotation has type `Q [Dec]`.
> - `[t| ... |]`, where the ”...” is a type; the quotation has type `Q Type`.
> - `[p| ... |]`, where the ”...” is a pattern; the quotation has type `Q Pat`.

とのことなので，`[e| ... |]` (ないし `e` を省略して単に `[| ... |]`) というクォートが参考になりそうだ．

文字列リテラル `"foo"` はどのような構文木で表されるかを確認するために，`[|e| "foo" ]` で生成される構文木を見てみよう．クォートで生成された構文木を出力するには，`runQ :: Language.Haskell.TH.Syntax.Quasi m => Q a -> m a` を使う．`IO` が `Quasi` のインスタンスになっているので，画面上に出力することができる．

```haskell
> :set -XTemplateHaskell

> import Language.Haskell.TH

> runQ [e| "foo" |]
LitE (StringL "foo")
```

ここで

- `StringL :: String -> Lit`
- `LitE :: Lit -> Exp`

である．つまり，あらかじめ計算しておいた `String` 型の値に `LitE . StringL` を適用すれば，所望の `Exp` が手に入ってしまうのである．思っていたよりも簡単そうだ[^2]．

構文木がどのようなプログラムになるのかは，`ppr` 関数を使えばわかる．

```haskell
> :l FizzBuzz.hs
[1 of 1] Compiling FizzBuzz         ( FizzBuzz.hs, interpreted )
Ok, one module loaded.

> ppr . LitE . StringL $ answer
"1\n\
\2\n\
\Fizz\n\
\4\n\
\Buzz\n\
\Fizz\n\
\7\n\
\8\n\
\Fizz\n\
\Buzz\n\
\11\n\
\Fizz\n\
\13\n\
\14\n\
\FizzBuzz\n\
\16\n\
\17\n\
\Fizz\n\
\19\n\
\Buzz\n\
\Fizz\n\
\22\n\
\23\n\
\Fizz\n\
\Buzz\n\
\26\n\
\Fizz\n\
\28\n\
\29\n\
\FizzBuzz\n\
\31\n\
\32\n\
\Fizz\n\
\34\n\
\Buzz\n\
\Fizz\n\
\37\n\
\38\n\
\Fizz\n\
\Buzz\n\
\41\n\
\Fizz\n\
\43\n\
\44\n\
\FizzBuzz\n\
\46\n\
\47\n\
\Fizz\n\
\49\n\
\Buzz\n\
\Fizz\n\
\52\n\
\53\n\
\Fizz\n\
\Buzz\n\
\56\n\
\Fizz\n\
\58\n\
\59\n\
\FizzBuzz\n\
\61\n\
\62\n\
\Fizz\n\
\64\n\
\Buzz\n\
\Fizz\n\
\67\n\
\68\n\
\Fizz\n\
\Buzz\n\
\71\n\
\Fizz\n\
\73\n\
\74\n\
\FizzBuzz\n\
\76\n\
\77\n\
\Fizz\n\
\79\n\
\Buzz\n\
\Fizz\n\
\82\n\
\83\n\
\Fizz\n\
\Buzz\n\
\86\n\
\Fizz\n\
\88\n\
\89\n\
\FizzBuzz\n\
\91\n\
\92\n\
\Fizz\n\
\94\n\
\Buzz\n\
\Fizz\n\
\97\n\
\98\n\
\Fizz\n\
\Buzz"
```

ここまでできたら，`putStrLn` の引数の部分に式 (`answerExpr :: Q Exp`) を接合すればおしまい．

## 感想

コンパイル時計算の仕組みとして Template Haskell がよくできすぎており，あまりに簡単に実現できてしまったので拍子抜けした．構文**木**とはいえど分岐すらしていない．次回はもう少し骨のあるトピックを用意したい．

## 脚注

[^1]: GHC stage restriction: ‘answerExpr’ is used in a top-level splice, quasi-quote, or annotation, and must be imported, not defined locally
[^2]: 欲しかったのは `Exp` ではなく `Q Exp` だが，`Q` は `Applicative` のインスタンスなので `pure` すればOK．
