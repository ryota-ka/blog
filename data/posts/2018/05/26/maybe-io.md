# Maybe と IO を一緒に使いたくなったら

たまには初学者向けにライトな話題を．

## 対象読者

- [すごいH本](http://learnyouahaskell.com/)を[12章](http://learnyouahaskell.com/a-fistful-of-monads)か[13章](http://learnyouahaskell.com/for-a-few-monads-more)ぐらいまでは読んだ
- `do` 構文を使って `IO` などの処理が書ける
- Haskell のプログラムはなんとなく書けるが，あまり綺麗に書けている気がしない

---

## `IO` の中で `Maybe` を使う

例として，以下のようなプログラムを考えてみよう．

- 2つの整数 `a` `b` を標準入力から1行ずつ順に読み込む
- `a` と `b` の和を標準出力に出力する
- 与えられた入力が整数でなかった場合には，その時点でエラーメッセージを出力し，プログラムを終了する

これらの要件を満たすプログラムを素朴に実装するならば，以下のようになるだろう．

```haskell
import System.Exit (die)
import Text.Read (readMaybe)

readInt :: String -> Maybe Int
readInt = readMaybe

main :: IO ()
main = do
    a <- readInt <$> getLine
    case a of
        Nothing -> die "not an integer"
        Just a' -> do
            b <- readInt <$> getLine
            case b of
                Nothing -> die "not an integer"
                Just b' -> print (a' + b')
```

さて，このプログラムを少し改変して，今度は3つの整数を受け取り，それらの和を出力するようにしたい．

```haskell
import System.Exit (die)
import Text.Read (readMaybe)

readInt :: String -> Maybe Int
readInt = readMaybe

main :: IO ()
main = do
    a <- readInt <$> getLine
    case a of
        Nothing -> die "not an integer"
        Just a' -> do
            b <- readInt <$> getLine
            case b of
                Nothing -> die "not an integer"
                Just b' -> do
                    c <- readInt <$> getLine
                    case c of
                        Nothing -> die "not an integer"
                        Just c' -> print (a' + b' + c')
```

4整数の場合であれば以下のようになるだろう．

```haskell
import System.Exit (die)
import Text.Read (readMaybe)

readInt :: String -> Maybe Int
readInt = readMaybe

main :: IO ()
main = do
    a <- readInt <$> getLine
    case a of
        Nothing -> die "not an integer"
        Just a' -> do
            b <- readInt <$> getLine
            case b of
                Nothing -> die "not an integer"
                Just b' -> do
                    c <- readInt <$> getLine
                    case c of
                        Nothing -> die "not an integer"
                        Just c' -> do
                            d <- readInt <$> getLine
                            case d of
                                Nothing -> die "not an integer"
                                Just d' -> print (a' + b' + c' + d')
```

延々とネストが深くなっていってしまうことがわかる．うまく抽象化できていない「臭い」がする．

## `Maybe` と `IO` の同居が問題なのではない

今しがた直面した**ネストが延々と深くなる問題**は，何も「`Maybe` と `IO` を同時に使いたいがために起こる」といった性質のものではない．例えば次のコードでは `Maybe` のみを用いており，`IO` は一切登場しない．

```haskell
safeDiv :: Int -> Int -> Maybe Int
safeDiv m n
  | n == 0    = Nothing
  | otherwise = Just (m `div` n)

example :: Maybe Int
example = case safeDiv 42 2 of
    Nothing -> Nothing
    Just a -> case safeDiv a 0 of
        Nothing -> Nothing
        Just b -> case safeDiv b 3 of
            Nothing -> Nothing
            Just c -> safeDiv c 7
```

先程見た例と同様，ネストが延々と深くなる構造を抱えており，**抽象化に失敗**している様子が見て取れる．

とはいえ，実際にこのようなコードが書かれることはまずあり得ない．なぜならば [`Maybe` は `Monad` 型クラスのインスタンスである](http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Maybe.html#t:Maybe)ので，`>>=` が利用できて (`Maybe` のための `>>=` の実装が与えられていて)，以下のように書き直すことができるからである．

```haskell
example' =
    safeDiv 42 2 >>= (\a ->
        safeDiv a 0 >>= (\b ->
            safeDiv b 3 >>= (\c ->
                safeDiv c 7
            )
        )
    )
```

更に do 構文を用いて書き直せば以下のようになる．

```haskell
example'' = do
    a <- safeDiv 42 2
    b <- safeDiv a 0
    c <- safeDiv b 3
    safeDiv c 7
```

立ちどころにネストが消えてしまった．

ここで改めて確認しておきたいのは，**適切な抽象化を行わなければ，`Maybe` だけを用いた場合でもネストが深くなり続ける問題は生じる**ということだ．言い換えると，冒頭の例が酷いコードになってしまったのは，何も `Maybe` と `IO` を同時に使おうとしたことそれ自体が直接の原因というわけではない．

では，ここで言う「適切な抽象化」とは何なのか？その答えは「`Monad` 型クラスのインスタンスになっていて，`>>=` が利用できること」である．

## `Monad` 型クラスによる抽象化

`Monad` 型クラスは，以下のように定義される[^1]型クラスである．

```haskell
-- GHC.Base より一部抜粋・可読性のため改変
class Applicative m => Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
```

「ある型 `m` をモナドとして振る舞わせたい」と思ったときには，この `m a -> (a -> m b) -> m b` という型を持つ演算子 `>>=` をその型に合った形で実装してやれば，それが叶うのだった．

「その型に合った形で実装してやれば」と書いたように，`Monad` のインスタンスになっている型によって `>>=` の実装は異なっている．`Maybe` `Either` `[]` `IO` などなど，`Monad` のインスタンスになっている型はいろいろあれど，(そもそも型が違うので当然だが)それぞれ異なる `>>=` の実装を持っている．このように，異なる型について同じ関数や演算子を用いることができる(ように見えるが実際には型によって違う実装が与えられている)多相を**アドホック多相 (_ad-hoc polymorphism_)** と呼ぶ．逆に言えば，単に `>>=` と書いた場合には，どの型に対する実装を指しているのかがわからないので，特に先程見た `Maybe` の実装を指していることを強調したい場合には `(>>=) @Maybe` と書くことにする[^2]．

`Maybe` は以下のように `>>=` を実装することで，`Monad` 型クラスのインスタンスになっている．

```haskell
-- GHC.Base より一部抜粋・可読性のため改変
instance Monad Maybe where
    Just x  >>= f = f x
    Nothing >>= _ = Nothing
```

左辺の値が `Just` であるか `Nothing` であるかによって条件分岐をしている．左辺が `Just x` であれば，中身の `x` の値を取り出して，「続きの計算」である `f` に渡す．左辺が `Nothing` の場合には，右辺の関数を使わず，その場で計算を中断する，という風に読める．この左辺の条件分岐は，**`Maybe` を返すような計算を直列に繋げる場合に，毎回 `case ~ of ...` を用いたパターンマッチで場合分けを行う部分を抽象化してくれる**．先程の `safeDiv` の例で，`(>>=) @Maybe` を導入した途端に `Just` と `Nothing` の場合分けが姿を消したのは，何を隠そう `(>>=) @Maybe` の実装の中にその場合分けが記述されているからなのであった．このように，繰り返し登場する常套句を一箇所にまとめて定義し再利用することは，日常のプログラミングにおいてごく当たり前のことだろう．

## `Maybe` と `IO` の両方の計算効果を持った型

さて，元々のモチベーションを思い出すと，冒頭の `Maybe` と `IO` が入り混じったコードをなんとか綺麗にしたいのだった．前節では，`Maybe` に対する `>>=` の実装が，単なるパターンマッチによる `Just` と `Nothing` の場合分けであることを見た．ここから得られる示唆は以下の様なものである．すなわち，適当に新しい型 `m` を作ってやって，その型を `Monad` 型クラスのインスタンスにする．(つまり `(>>=) @m` の実装を書く．) その際に，一連の計算を途中で失敗させることができる `Maybe` と，副作用を引き起こすことができる `IO` の，両方の計算効果を持たせるような抽象化を施してやればいいのではないだろうか．つまり，`x >>= f` の実装として，

- IO アクション `x` を実行する
  - 例: 標準入力から1行読み込み，整数であれば `Just n` を，そうでなければ `Nothing` を返すような IO アクション
- 実行結果は `Maybe a` 型を持つので，その値が
  - `Nothing` のときは `pure Nothing` を返し，その場で計算を終了する
  - `Just y` のときは，続きの計算である `f` に `y` を渡して計算を続ける
    - `f y` は IO アクションでこれを実行した結果は `Maybe b` 型を持つので…… (以下同様)

というようなものを与えてやる．こうした実装を与えてやった上で `>>=` を使えば，`x >>= f >>= g >>= h >>= ...` といった形で，好きなだけ計算を続けることができるし，`f` `g` `h` がそれぞれ計算結果として `Nothing` を返した場合[^3]には，そこで計算を打ち止めにする，といったことが可能になるはずである．

では実際に，次のようなデータ型を定義してみよう．

```haskell
newtype MaybeIO a = MaybeIO { runMaybeIO :: IO (Maybe a) }
```

ここで `newtype` についての注意をしておく．既に馴染み深い読者は読み飛ばしてしまって構わない．

`newtype` とは，あるデータ型に対して，元の型と[同型 (_isomorphic_)](https://ja.wikipedia.org/wiki/同型写像) であるような新しい型を定義するための機能である．実際，

```haskell
-- コンストラクタ (型ではない)
MaybeIO :: IO (Maybe a) -> MaybeIO a

-- フィールド名
runMaybeIO :: MaybeIO a -> IO (Maybe a)
```

であり，

```haskell
MaybeIO . runMaybeIO == id
runMaybeIO . MaybeIO == id
```

であるので，`MaybeIO a` は，実質的に `IO (Maybe a)` と同一視してしまって構わない．では，なぜ元の型 (`IO (Maybe a)`) と同一視できるはずの新しい型 (`MaybeIO a`) を定義するのだろうか？`type MaybeIO a = IO (Maybe a)` でシノニムを与えるだけで十分ではないのか？実は，このあとこの型を `Functor` `Applicative` `Monad` のインスタンスにするのだが，Haskell 2010 では `type` キーワードで定義した型シノニムに対して，インスタンスの宣言ができないのである[^4]．`newtype` で定義された型は，型検査時には元の型とは別の型として扱われるので，ある型クラスのインスタンスにしたい場合に便利なことが多い．実行時には元の型と同一のものとして扱われるので，オーバーヘッドは発生せず，抽象化によるペナルティを受けない．

`newtype` を用いたコードベースは，辻褄合わせのために `MaybeIO` やら `runMaybeIO` やらがたくさん登場して，初学者にとってはあまり読みやすいものではないかもしれない．しかしながら，先程も説明したとおり，`MaybeIO a` と `IO (Maybe a)` は同一視できるので，`MaybeIO` や `runMaybeIO` は特別何かをしているわけではなく，ただ型を合わせるために辻褄合わせをしているだけである．読んでいて混乱した場合には，これらがどのような型を持つかを冷静に見つめ直した上で，どの項がどのような型を持つかの把握に努めてほしい．

- `MaybeIO :: IO (Maybe a) -> MaybeIO a`
  - `IO (Maybe a)` を型クラスのインスタンスにするためのラッピング
- `runMaybeIO :: MaybeIO a -> IO (Maybe a)`
  - 中身を取り出せば `IO` の世界で使える

では，実際に `MaybeIO` を `Monad` 型クラスのインスタンスにしてみよう．可読性のために，メソッドの宣言時に型シグネチャを記述しているが，これは `InstanceSigs` 言語拡張を有効にすることで可能になる．

```haskell
instance Functor MaybeIO where
    fmap :: (a -> b) -> MaybeIO a -> MaybeIO b
    -- do ブロック全体は IO (Maybe b) 型を持ち
    -- これを MaybeIO コンストラクタに渡すことで
    -- 右辺全体が MaybeIO b 型になることに注意
    fmap f action = MaybeIO $ do -- IO の do
        -- action は MaybeIO a 型なので
        -- runMaybeIO で IO (Maybe a) にすれば IO の do の中で使える
        x <- runMaybeIO action -- IO (Maybe a) から <- で 取り出した x は Maybe a 型を持つ
        case x of
            Nothing -> pure Nothing
            Just x' -> pure (Just (f x'))

instance Applicative MaybeIO where
    pure :: a -> MaybeIO a
    pure x = MaybeIO (pure (Just x)) -- 右辺の pure は IO の pure

    (<*>) :: MaybeIO (a -> b) -> MaybeIO a -> MaybeIO b
    lhs <*> rhs = MaybeIO $ do -- やはり IO の do
        mf <- runMaybeIO lhs -- IO に変換してから <- で取り出す
        case mf of
            Nothing -> pure Nothing
            Just f -> do
                mx <- runMaybeIO rhs -- IO に変換してから <- で取り出す
                case mx of
                    Nothing -> pure Nothing
                    Just x -> pure (Just (f x))

instance Monad MaybeIO where
    (>>=) :: MaybeIO a -> (a -> MaybeIO b) -> MaybeIO b
    lhs >>= rhs = MaybeIO $ do -- やはり IO の do
        x <- runMaybeIO lhs
        case x of
            Nothing -> pure Nothing
            Just x' -> runMaybeIO (rhs x')
```

「IOアクションの実行」と「`Just` / `Nothing` に対するパターンマッチ」の両方を行うコードをそのまま書き下した．このような抽象化を用いれば，冒頭の例は，以下のように書き換えられる．

```haskell
main :: IO ()
main = do -- IO の do
    -- do ブロック全体は MaybeIO Int 型を持ち
    -- runMaybeIO で IO (Maybe Int) に変換していることに注意
    msum <- runMaybeIO $ do -- MaybeIO の do
        a <- MaybeIO (readInt <$> getLine)
        b <- MaybeIO (readInt <$> getLine)
        pure (a + b)
    case msum of -- msum は Maybe Int 型をもつ
        Nothing -> die "not an integer"
        Just sum -> print sum
```

読み込む行数を2行から4行に変更してもこの通り．

```haskell
main :: IO ()
main = do
    msum <- runMaybeIO $ do
        a <- MaybeIO (readInt <$> getLine)
        b <- MaybeIO (readInt <$> getLine)
        c <- MaybeIO (readInt <$> getLine)
        d <- MaybeIO (readInt <$> getLine)
        pure (a + b + c + d)
    case msum of
        Nothing -> die "not an integer"
        Just sum -> print sum
```

## `Maybe` と他のモナドを一緒に使いたくなったら

自分で `MaybeHoge` 型を定義して，`Functor` `Applicative` `Monad` のインスタンス宣言を書いてもらって……ではあまりに面倒なので，よりよい抽象化の方法を考えたい．

先程 `MaybeIO` を `Functor` `Applicative` `Monad` のインスタンスにした際のコードをもう一度見返してみよう．

```haskell
instance Functor MaybeIO where
    fmap f action = MaybeIO $ do
        x <- runMaybeIO action
        case x of
            Nothing -> pure Nothing
            Just x' -> pure (Just (f x'))

instance Applicative MaybeIO where
    pure :: a -> MaybeIO a
    pure x = MaybeIO (pure (Just x))

    lhs <*> rhs = MaybeIO $ do
        mf <- runMaybeIO lhs
        case mf of
            Nothing -> pure Nothing
            Just f -> do
                mx <- runMaybeIO rhs
                case mx of
                    Nothing -> pure Nothing
                    Just x -> pure (Just (f x))

instance Monad MaybeIO where
    lhs >>= rhs = MaybeIO $ do
        x <- runMaybeIO lhs
        case x of
            Nothing -> pure Nothing
            Just x' -> runMaybeIO (rhs x')
```

よくよく見てみると，このコード中には `getLine :: IO String` や `unsafePerformIO :: IO a -> a` など，`IO` に関わるものはまったく登場しない．このコードが語るのは，**何かしらのモナド `m` の中に `Maybe a` が入っている**ということだけであり，`IO` という型自体にはまったく依存していないことがわかる．`MaybeIO` という名前をつけてみたものの，実は `IO` だけではなく，好きなモナド `m` と `Maybe` を組み合わせることができるようになっていたようだ．

さて，`IO` 以外にも対応していた，ということがわかったので，`MaybeIO` よりももう少し良い名前を付けてあげたい．これは巷では `MaybeT` という名前で呼ばれている．このように，あるモナドの中に別のモナドを入れて，両方の(あるいは3つ以上の)計算効果を同時に扱うパターンは**モナド変換子** (monad transformer) と呼ばれている．`MaybeT` の `T` は Transformer に由来する．

```haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
```

参考までに，`m = IO` とした場合には，

```haskell
MaybeT IO a = MaybeT { runMaybeT :: IO (Maybe a) }
```

となる．

```haskell
newtype MaybeIO = MaybeIO { runMaybeIO :: IO (Maybe a) }
```

だったことを思い出すと，`IO` が任意の `m` に抽象化されただけであることがわかる．

このように定義された `MaybeT` 型は，[`transformers`](https://hackage.haskell.org/package/transformers) というライブラリによって[提供されている](https://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-Maybe.html#t:MaybeT)．`IO` だけでなく，任意の `Monad m => m` について `MaybeT m` が `Monad` 型クラスのインスタンスになっていることにより，ライブラリのユーザが好きなモナドを自由に組み合わせて使える柔軟性を獲得している．

`transformers` ライブラリを利用すれば，冒頭のコードは最終的に以下のように書くことができる．

```haskell
#!/usr/bin/env stack
-- stack runhaskell --resolver lts-11.10 --package transformers

import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import System.Exit (die)
import Text.Read (readMaybe)

readMaybeInt :: IO (Maybe Int)
readMaybeInt = readMaybe <$> getLine

main :: IO ()
main = do
    msum <- runMaybeT $ do
        a <- MaybeT readMaybeInt
        b <- MaybeT readMaybeInt
        c <- MaybeT readMaybeInt
        d <- MaybeT readMaybeInt
        pure (a + b + c + d)
    case msum of
        Nothing -> die "not an integer"
        Just sum -> print sum
```

さて，このように構成したモナド変換子 (_monad transformer_) 自体を抽象化するものとして，[`MonadTrans` 型クラス](https://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-Class.html#t:MonadTrans)が提供されている．このメソッドたる `lift` 関数を抑えておけば，モナド変換子を習得したと言ってよいだろう．

## 終わりに

`Maybe` と `IO` を一緒に使いたくなったら，それはあなたがモナド変換子を訪ねるきっかけかもしれない．

[^1]: http://hackage.haskell.org/package/base-4.11.1.0/docs/src/GHC.Base.html#Monad
[^2]: この記法は `TypeApplications` という GHC 拡張のもとで有効．
[^3]: 厳密に言えば，これらの関数が返す IO アクションの実行結果が `Nothing` であるとき
[^4]: `TypeSynonymInstances` 拡張を有効にすれば可能．
