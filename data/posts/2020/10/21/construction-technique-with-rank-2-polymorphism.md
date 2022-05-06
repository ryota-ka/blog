---
keywords:
  - Dhall
  - Haskell
  - Rank-2 polymorphism
---

# 引数で受け取った値を通じてのみ値を構築することを強制するランク2多相を使ったテクニック

Dhall の Prelude を眺めていて見付けた，ランク2多相を使ったテクニックを紹介する．

---

Dhall のビルトイン型の一部 (e.g. `Natural` `Option` `List`) にはそれぞれ `build` という関数が定義されている．これはコメントで "inverse of `fold`" と説明されているから，最も普遍的に値を構築する手段として提供されているようである．

例えば，`Natural/build` 関数の型は以下のようになっている．

```dhall
⊢ :t Natural/build

( ∀(natural : Type) → ∀(succ : natural → natural) → ∀(zero : natural) → natural
) →
  Natural
```

引数として受け取った関数を元に `Natural` 型の値を構築する関数であることがわかる．引数として受け取る関数の型は ` ∀(natural : Type) → ∀(succ : natural → natural) → ∀(zero : natural) → natural` である．このような型を持つ関数 `mkThree` を定義してみよう．

```dhall
⊢ :let mkThree = λ(natural : Type) → λ(succ : natural → natural) → λ(zero : natural) → succ (succ (succ zero))

  mkThree
: ∀(natural : Type) → ∀(succ : natural → natural) → ∀(zero : natural) → natural
```

`Natural/build` を `mkThree` 関数に適用すると，無事に `3` という値を構築することができる．

```dhall
⊢ Natural/build mkThree

3
```

さて，Dhall には自然数のリテラルが存在するので，`succ (succ (succ zero))` などと回りくどい記述をする代わりに，直接 `3` と書けないのだろうか．実際に確かめてみると，残念ながらこの試みは失敗に終わる．

```dhall
⊢ Natural/build (λ(natural : Type) → λ(_ : natural → natural) → λ(_ : natural) → 3)

Error: Wrong type of function argument

  …
→ …
→ …
→ - _@2
  + Natural

1│ Natural/build (λ(natural : Type) → λ(_ : natural → natural) → λ(_ : natural) → 3)

(input):1:1
```

型 `_@2` と `Natural` が一致していないというエラーが得られた．`_@2` の `2` は de Bruijn index で，最も外側のラムダ抽象で導入された (型) 変数 `natural` を指している．値 `3` は大文字の `Natural` 型を持つが，これが全称量化された型変数 `natural` と一致しないのである．このラムダ抽象の本体は `natural` 型の値を返さねばならず，そのような値は (`natural` が全称量化されているがゆえに) `succ : natural → natural` と `zero : natural` を通じてのみしか構築することができないのだ．

同様の関数を Haskell で定義してみよう．

```haskell
{-# LANGUAGE Rank2Types #-}

import GHC.Natural (Natural)

build :: (forall natural. (natural -> natural) -> natural -> natural) -> Natural
build f = f (+ 1) 0
```

生産者は，第1引数に `(+ 1) :: Natural -> Natural` を，第2引数に `0 :: Natural` を供給する．しかし，消費者の立場から見れば，量化された `natural` 型にしかアクセスできないので，内部実装に `Natural` が使われていることなど知る由もない．その結果，引数で受け取った `succ` と `zero` のみを使って値を構築することしかできないのである．

```haskell
mkThree :: forall natural. (natural -> natural) -> natural -> natural
mkThree succ zero = succ (succ (succ zero))

three :: Natural
three = build mkThree -- 3
```

このテクニックの応用例を考えてみたが，残念ながら特に何も思い付かなかった．ライブラリを設計する際，syntactic な construction method だけをユーザに提供したいというモチベーションがあるならば，パターンとして頭の片隅に置いておいて損はないかもしれない．
