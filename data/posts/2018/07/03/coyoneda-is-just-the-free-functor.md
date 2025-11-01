---
keywords:
  - Coyoneda
  - Haskell
---

# Coyonedaって…… お前functorがデータ構造になっただけやんけ！！

[operational](https://wiki.haskell.org/Operational)（あるいは freer）と呼ばれているものの説明として，

- a) `Coyoneda`を使うと，kindが`* -> *`であるような任意の型からfunctorを作り出せる
  - 任意の型`f :: * -> *`について`Coyoneda f`は`Functor`のインスタンスになる
- b) `Free`を使うと，任意のfunctorからmonadを作り出せる
  - `Functor`のインスタンスである任意の型`f`について`Free f`は`Monad`のインスタンスになる
- aとbを組み合わせると，適当な型`f :: * -> *`からmonadを作り出せて便利〜🙌

というストーリーが往々にして語られる[^1]．

---

Freeについては既に多くの解説が存在するので，詳しい解説は他の記事をあたってもらうこととして，インフォーマルな説明をしておくと，以下のような気持ちを持ったデータ構造である．

- a) monadを特徴付けるのは`fmap`+`pure`+`join`である
- b) `data Free f a = Pure a | Join (f (Free f a))`と定義されるデータ構造は，その構造内に`pure`と`join`を内包している
  - 実際，
    - `Pure :: a -> Free f a`
    - `Join :: f (Free f a) -> Free f a`
- aとbを組み合わせると，`f`がfunctorである（`fmap`が実装されている）ときに，`Free f a`は`fmap`, `pure`, `join`を備えているのでmonadと言っても過言ではない👏

補足: `Join`というコンストラクタは，場合によっては`Free`だとか`Impure`だとか書かれる場合があるが，これがまさに`Join`と名付けられていることが，私の（インフォーマルな）理解の手助けになった．人々は親切なネーミングを心掛けてほしい．

繰り返しになるが，以上はインフォーマルな気持ちである．フォーマルな説明を求めると，monadからfunctorへの忘却関手の左随伴だとか何とか言われて，もう勘弁してくれという気持ちになるので，お近くの圏論に詳しい方に聞いてください．

`Free f a`が（`f`がfunctorであるときに）monadになりそうだということはわかったので，次は`Coyoneda`を見てみよう．

```haskell
 data Coyoneda f a = forall x. Coyoneda (x -> a) (f x)
```

最初に見たときは，こいつがfunctorになると言われても意味不明だったが，少し書き換えると理解の助けになる．`a`を`b`に，`x`を`a`に置き換えてみると，

```haskell
data Coyoneda f b = forall a. Coyoneda (a -> b) (f a)
```

GADTで書けば[^2]，

```haskell
data Coyoneda f b where
    Coyoneda :: (a -> b) -> f a -> Coyoneda f b
```

**いやいやいや，お前ほぼ`fmap :: (a -> b) -> f a -> f b`やんけ！！！**

というわけで，Coyonedaのインフォーマルな気持ちとしては，`Free`が自身にmonadっぽみを内包しているが故にmonadとして振る舞えるのと同様に，`Coyoneda`は自身にfunctorっぽみを内包しているが故にfunctorとして振る舞えるのであった．

もちろん，データ構造それ自身が「functorっぽい」からといって，本当にfunctorとして振る舞うのかは自明ではないが，米田の補題によって保証される，らしい．詳しくはお近くの圏論に詳しい方に聞いてください．

実際，

```haskell
foo :: Functor f => Coyoneda f a -> f a
foo (Coyoneda f x) = f <$> x

bar :: f a -> Coyoneda f a
bar x = Coyoneda id x
```

と置くと，

```haskell
foo . bar == id
bar . foo == id
```

と全単射が存在して安心．

free monadはmonadが持つ性質をデータ構造で表現したものであることと同様に，Coyonedaはfunctorが持つ性質をデータ構造で表現したものであることを見た．これはインフォーマルにはfree functorとでも呼んでいい構造かもしれない[^3]．同じようにして，free applicative functorなる構造も考えることができそうだ．

```haskell
data FreeApplicative f b
    = Pure b
    | forall a. Ap (f (a -> b)) (FreeApplicative f a)
```

と定義すると，以下のような対応が取れる．

| applicative                      | free applicative                                                 |
| -------------------------------- | ---------------------------------------------------------------- |
| `pure :: b -> f b`               | `Pure :: b -> FreeApplicative f b`                               |
| `ap :: f (a -> b) -> f a -> f b` | `Ap :: f (a -> b) -> FreeApplicative f a -> FreeApplicative f b` |

GADTならこれを直接書き下して定義とすることもできる．

```haskell
data FreeApplicative f b where
    Pure :: b -> FreeApplicative f b
    Ap :: f (a -> b) -> FreeApplicative f a -> FreeApplicative f b
```

free applicativeが何の役に立つのかは知らないが，arXivで論文を見かけた気がする．そもそもDay convolutionとはなんですか．

## 脚注

[^1]: 少なくとも2年ぐらい前にlotz先生に聞いたときはそうだった．
[^2]: 実際[kan-extensions](https://hackage.haskell.org/package/kan-extensions/)では[GADTで定義されている](https://hackage.haskell.org/package/kan-extensions-5.1/docs/src/Data-Functor-Coyoneda.html#Coyoneda)
[^3]: 実際にfree functorと呼ばれるものについては[nLab](https://ncatlab.org/nlab/show/free+functor)などを参照．
