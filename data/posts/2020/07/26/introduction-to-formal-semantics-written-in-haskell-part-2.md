---
keywords:
  - Effect system
  - Formal semantics
  - Haskell
---

# 『形式意味論入門』を Haskell に書き下す（後編）

前編はこちら

https://blog.ryota-ka.me/posts/2019/08/15/introduction-to-formal-semantics-written-in-haskell-part-1

---

## §8 発話文脈・変項割当

$$
\textit{She loves him.}
$$

という文を発話したとする．この文は，代名詞$\textit{she}$と$\textit{him}$の2つの指示対象が発話の状況から明らかなときにのみ適切である．このように，文脈に依存して意味が初めて定まる言語表現を**直示**（deixis）という．指示対象を明らかにするため，次のように代名詞に指標（index）を振ることにする．

$$
\textit{She}_\mathrm{1} \textit{ loves } \textit{him}_\mathrm{2}.
$$

${\textit{She}}$と${\textit{him}}$に$1$と$2$という異なる指標が割り当てられているのは，それらが相異なる外延をもつことを表している．

既に述べたように，この文の外延は$\textit{She}_\mathrm{1}$および$\textit{him}_\mathrm{2}$の**指示対象が文脈から定まることで初めて計算することができる**．このような文は自由変項[^1]を含んだ**開いた項**（open term）として見做すことができる．このような発話に際しての状況をutterance contextと呼ぶ．定訳があるかはわからないので，この記事中では**発話文脈**と訳すことにする．

発話文脈を形式的に扱う道具立てとして，**変項割当**（variable assignment）を導入する．Heim and Kratzerでは，変項割当を以下のように定義している[^2]．

> （変項）割当とは，$\mathbb{N}$（自然数の集合）から$D_e$への部分関数である．

例えば，以下の$g, g', g''$はいずれも$\text{Model PEANUTS}$における変項割当である．

$$
\begin{align*}
g & =
\left [
  \begin{aligned}
    & 1 \mapsto \text{Snoopy} \\
    & 2 \mapsto \text{Woodstock}
  \end{aligned}
\right ] \\
g' & =
\left [
  \begin{aligned}
    & 42 \mapsto \text{Snoopy}
  \end{aligned}
\right ] \\
g'' & = \phi
\end{align*}
$$

直示を含む項の意味計算をするには，指示対象を発話文脈に問い合わせる必要があるため，**ある変項割当のもとで項の意味計算を行なう**必要がある．そこで，変項割当$g$のもとでの項$e$の外延を$\llbracket e \rrbracket^g$と表記することにする．以降$\llbracket \cdot \rrbracket$は$\llbracket \cdot \rrbracket^\phi$の略記であるとする[^3]．

また，**変項**（variable）という概念は形式的には次のように定義される[^4][^5]．

> 終端記号$\alpha$は，$\llbracket \alpha \rrbracket^g \neq \llbracket \alpha \rrbracket^{g'}$となるような変項割当$g$および$g'$が存在するとき，かつそのときに限り変項であるという．

変項は**自由変項**（free variable）と**束縛変項**（bound variable）に分類される[^6]が，ある項が自由変項を含む開いた項であるか，それとも自由変項を含まない**閉じた項**（closed term）であるかを区別することは重要である．閉じた項の意味計算はその項の内部だけで完結する一方，開いた項の意味計算を行うためには，変項の指示対象を外部から与える必要があるからである．

### §8のまとめ

- 発話文脈に依存して意味が初めて定まる言語表現を直示という
- 発話文脈を形式的に扱うために変項割当という概念を導入する
  - これは$\mathbb{N}$（自然数の集合）から$D_e$（個体の集合）への部分関数である
- 直示を含む文の意味計算は変項割当をともなって行なう

## §9. 新しい言語

さて，変項割当をHaskell上で表現してみよう．変項割当は自然数の集合$\mathbb{N}$から個体の集合$D_e$への部分関数だったが，項レヴェルの計算で部分関数を扱いたくないので，型レヴェルで扱うことにする．型レヴェルで扱うことにより，適切でない文脈で発話がなされたことを型エラーとして検知することができる．

変項割当を導入したモチベーションは，発話文脈を形式的に扱うことであった．文脈上に存在する値を参照するような計算は，Haskellでは`Reader`として知られている．ただし，今我々が欲しいのは単なる`Reader`ではなく，型レヴェル自然数を指定した際，その自然数に対応するような値を取ってくることのできる`Reader`である．このような挙動を実現するため，[`membership`](https://hackage.haskell.org/package/membership)および[`extensible-skeleton`](https://hackage.haskell.org/package/extensible-skeleton)packageを用い，extensible effectsを用いて対象言語を構築することにする．

自然数と個体の対応関係を表す型`index |-> entity`を以下のように定義する．ここで`index`は型レヴェル自然数である，すなわち`Nat`種をもつ．

```haskell
import GHC.Types (Type)
import GHC.TypeNats (Nat)
import Data.Extensible.Effects (ReaderEff)
import Type.Membership (type (>:), Assoc, Lookup)

type (|->) :: Nat -> Type -> Assoc Nat (Type -> Type)
type index |-> entity = index >: ReaderEff entity
```

例えば`1 |-> PEANUTS`という型は，指標`1`に`PEANUTS`型の値が割り当てられていることを意味する．

前編で定義した`Model`言語を書き換え，extensible effectsに埋め込むことにする．この言語では，項が`Eff ctx a`という型で表現される．ただし，[`Eff`型は`extensible-skeleton`packageで定義されている](https://hackage.haskell.org/package/extensible-skeleton/docs/Data-Extensible-Effect.html#t:Eff)．ここで`ctx`は`ctx :: [Assoc Nat (Type -> Type)]`という種をもち，具体的には`ctx = '[1 |-> PEANUTS, 2 |-> PEANUTS]`のような型になる．`e :: Eff ctx a`は直観的には「`ctx`という文脈を伴った`a`型の項`e`」と読むことができる．

さて，前編で導入したFAとPMという2つの意味計算規則を，この新しい言語に沿う形で再び埋め込む必要がある．

FAはHaskellにおける通常の関数適用として埋め込まれる．前編と同様に`is_a`という関数を定義しておく．

```haskell
is_a :: a -> (a -> b) -> b
is_a x f = f x
```

また，PMは以下のように再定義される．

```haskell
import Data.Extensible.Effect (Eff)

predicateModification
    :: (a -> Eff ctx Bool) -- ^ λx. p(x)
    -> (a -> Eff ctx Bool) -- ^ λx. q(x)
    -> (a -> Eff ctx Bool) -- ^ λx. p(x) ∧ q(x)
predicateModification p q = \x -> do
    p' <- p x
    q' <- q x
    pure $ p' && q'
```

また，前編で定義した$boy$や$love$といった語彙は以下のように再定義される．

```haskell
boy :: PEANUTS -> Eff ctx Bool
boy x = pure $ case x of
    Charlie   -> True
    Linus     -> True
    Schroeder -> True
    _         -> False

love :: PEANUTS -> Eff ctx (PEANUTS -> Eff ctx Bool)
love object = do
    pure $ \subject -> pure $ case (subject, object) of
        (Lucy , Schroeder) -> True
        (Sally, Linus    ) -> True
        (Patty, Charlie  ) -> True
        _                  -> False
```

例として，$\textit{Snoopy is a cute dog.}$の意味計算は以下のように行われる．ただし`cute`および`dog`は前編で定義したものを適切に再定義したものとする．

```haskell
sentence :: Eff '[] Bool
sentence = Snoopy `is_a` cuteDog
  where
    cute, dog, cuteDog :: PEANUTS -> Eff ctx Bool
    cute = _
    dog = _
    cuteDog = predicateModification cute dog

main :: IO ()
main = hspec $ do
    describe "Snoopy is a cute dog" $ do
        it "is false" $ do
            let denotation = leaveEff sentence
            denotation `shouldBe` False
```

ここで[`leaveEff :: Eff '[] a -> a`](https://hackage.haskell.org/package/extensible-skeleton/docs/Data-Extensible-Effect.html#v:leaveEff)という関数が登場しているが，これは文脈が空（すなわち`ctx = '[]`）であるような場合にのみ，effectfulな計算から中身を取り出すことのできる関数である．

### §9のまとめ

- 変項割当を型安全に扱うために，以降は`Eff ctx a`という言語を用いる
  - これは発話文脈`ctx`を伴った`a`型と読める

## §10 Traces & Pronouns (T&P)

下準備が随分長くなってしまったが，いよいよ${\textit{She}_\mathrm{1} \textit{ loves } \textit{him}_\mathrm{2} \text{.}}$という直示表現を含んだ文の意味計算を行うために，第3の意味計算規則である**Traces & Pronouns** (T&P)を導入する[^7]．

> $\alpha_i$が代名詞または痕跡で，$g$が変項割当であり，$i$が$g$の定義域に含まれるとき，$\llbracket \alpha_i \rrbracket = g(i)$である．

$\alpha_i$はそれ単体では意味が定まらないが，変項割当$g$によって指示対象が指定されているときに，$g$を用いて意味計算を行なうことができる，ということを表している．ここでは例として

$$
g =
\left [
\begin{aligned}
& 1 \mapsto \text{Lucy} \\
& 2 \mapsto \text{Schroeder}
\end{aligned}
\right ]
$$

なる変項割当$g$のもとで${\textit{She}_\mathrm{1} \textit{ loves } \textit{him}_\mathrm{2} \text{.}}$の意味計算を行ってみよう．

$$
\begin{align*}
\llbracket \textit{She}_\mathrm{1} \textit{ loves } \textit{him}_\mathrm{2} \rrbracket
&= \llbracket \textit{love him}_\mathrm{2} \rrbracket^g(\llbracket \textit{she}_\mathrm{1} \rrbracket^g) & (\because \text{FA}) \\
&= \llbracket \textit{love} \rrbracket^g(\llbracket \textit{him}_\mathrm{2} \rrbracket^g)(\llbracket \textit{she}_\mathrm{1} \rrbracket^g) & (\because \text{FA}) \\
&= (\lambda x y. y \text{ loves } x)(\llbracket \textit{him}_\mathrm{2} \rrbracket^g)(\llbracket \textit{she}_\mathrm{1} \rrbracket^g) & (\because \text{lexicon}) \\
&= (\lambda x y. y \text{ loves } x)(g(2))(g(1)) & \color{red}{(\because \text{T\&P})} \\
&= (\lambda x y. y \text{ loves } x)(\text{Schroeder})(\text{Lucy}) & (\because \text{definition of } g) \\
&= (\lambda y. y \text{ loves Schroeder} )(\text{Lucy}) & (\because \beta \text{-reduction}) \\
&= \text{Lucy loves Schroeder} & (\because \beta \text{-reduction}) \\
&= \text{True}
\end{align*}
$$

さて，このT&PはHaskellでは以下のように実装できる．ただし前述の通り，変項割当は値レヴェルではなく型レヴェルで表現する．

```haskell
import Data.Kind (Type)
import Type.Membership (Assoc)

traceOrPronoun
    :: forall (index :: Nat) (entity :: Type) (ctx :: [Assoc Nat (Type -> Type)])
     . Lookup ctx index (ReaderEff entity)
    => Eff ctx entity
traceOrPronoun = askEff (Proxy @index)
```

ここで型変数`index :: Nat`は，代名詞または痕跡に与えられた指標（型レヴェル自然数）である．`entity :: Type`はその文において考えているモデルに存在する個体の型を表す型変数であり，具体的には`PEANUTS`型などが代入される．`ctx :: '[Assoc Nat (Type -> Type)]`はこの発話がなされた発話文脈，すなわち変項割当である．また，`Lookup ctx index (ReaderEff entity)`というconstraintは，この変項割当に対する要請を表している．どのような要請かというと，指標`index`に対応する個体が定められていることを要求しているのである．

constraintを無視して`=>`の後ろだけに注目すれば`Eff ctx entity`という単純な型をしており，（`ctx`というeffectsを伴った）個体の型を表していることが見て取れる．また，実装部分は`askEff Proxy`と非常にシンプルで，`ReaderEff entity`というeffectを発生させているだけであることがわかる．ただしこのeffectは型レヴェル自然数`index`に対応付けられている．

`traceOrPronoun`関数を用いて$\textit{She}_\mathrm{1} \textit{ loves } \textit{him}_\mathrm{2} \textit{.}$をHaskellに埋め込むと次のようになる．

```haskell
import Data.Function ((&))

-- | "She(1) loves him(2)."
sentence :: Eff '[1 |-> PEANUTS, 2 |-> PEANUTS] Bool
sentence = do
    she      <- traceOrPronoun @1
    him      <- traceOrPronoun @2
    lovesHim <- love him
    she & lovesHim
```

このように組み立てた文の意味計算は[`runReaderEff`](https://hackage.haskell.org/package/extensible-skeleton/docs/Data-Extensible-Effect.html#v:runReaderEff)関数および前述の[`leaveEff`](https://hackage.haskell.org/package/extensible-skeleton-0/docs/Data-Extensible-Effect.html#v:leaveEff)関数を用いて行う．

```haskell
runReaderEff
    :: Eff ((index |-> PEANUTS) : ctx) a -- ^ ctxの先頭の@index |-> PEANUTS@というeffectを剥がす
    -> PEANUTS -- ^ 代わりに指標indexに対応する個体を外から与える
    -> Eff ctx a
```

実際にHaskell上で意味計算を行う様子を以下に示す．

```haskell
sentence :: Eff '[1 |-> PEANUTS, 2 |-> PEANUTS] Bool
sentence = do
    she1      <- traceOrPronoun @1
    him2      <- traceOrPronoun @2
    lovesHim2 <- love him2
    she1 & lovesHim2

main :: IO ()
main = hspec $ do
    describe "She(1) loves him(2)" $ do
        it "is true when she(1) refers Lucy and him(2) refers Schroeder" $ do
            let eval = leaveEff
                     . flip (runReaderEff @2) Schroeder
                     . flip (runReaderEff @1) Lucy
                denotation = eval sentence

            denotation `shouldBe` True
```

異なる変項割当のもとで意味計算を行った場合，その計算結果は当然違ったものになる．

```haskell
        it "is false when she(1) refers Lucy and him(2) refers Charlie" $ do
            let eval = leaveEff
                     . flip (runReaderEff @2) Charlie
                     . flip (runReaderEff @1) Lucy
                denotation = eval sentence

            denotation `shouldBe` False
```

仮に不十分な発話文脈のもとで文が発話された場合，すなわち`runReaderEff`関数で与えた変項割当が不十分である場合，コンパイルに成功しない．`leaveEff :: Eff '[] a -> a`関数は`ctx`が空（`'[]`）であることを要求するため，引数にeffectが残存している場合には型が合わないためである．このように変項割当を型レヴェルで扱うことにより，発話文脈に対する型安全性を獲得することができる．

### §10のまとめ

- T&Pという意味計算規則が導入された
  - $\alpha_i$が代名詞または痕跡で，$g$が変項割当であり，$i$が$g$の定義域に含まれるとき，$\llbracket \alpha_i \rrbracket = g(i)$である．
- 指標を型レヴェルで扱えば，T&Pは`askEff`関数によって表現でき，文脈上の具体的な指示対象は`runReaderEff`関数によって与えられる

## §11 Predicate Abstraction (PA)

関係代名詞を含んだ文の意味計算をしたい．以下に関係代名詞$\textit{who}$を含んだ文の例を挙げる．

$$
\textit{The boy who Lucy loves } t \textit{ is a player.}
$$

ただし$t$は関係代名詞$\textit{who}$が移動した痕跡（trace）である．これらは指示対象としては同じものを指すので，同じ指標を与えることにする．

$$
\textit{The boy } \textit{who}_\mathrm{1} \textit{ Lucy loves } \textit{t}_\mathrm{1} \textit{ is a player}.
$$

また，`player`はHaskell上で以下のように定義しておく．

```haskell
player :: PEANUTS -> Eff ctx Bool
player x = pure $ case x of
    Snoopy    -> True
    Charlie   -> True
    Lucy      -> True
    Linus     -> True
    Patty     -> True
    Schroeder -> True
    _         -> False
```

さて，この文は次のような統語構造を持っている．

![syntactic tree](https://gyazo.com/a4b4f8b109a75478c7cfc729d8294cca.png)

[合成性原理（principle of compositionality）](https://en.wikipedia.org/wiki/Principle_of_compositionality)を根拠に，この構文木の部分木について考察することは有意義である．そこで，まずこの文の主部である$\textit{The boy } \textit{who}_\mathrm{1} \textit{ Lucy loves } t_\mathrm{1}$について考えてみよう．定冠詞$\textit{the}$が付いていることから，この名詞節の意味はただ一つに定まる個体であることが予想される．つまり

$$
\textit{The boy } \textit{who}_\mathrm{1} \textit{ Lucy loves } t_\mathrm{1} : e
$$

である．

この個体は発話文脈によらず一定である，すなわちこの名詞句は自由変項を含まない閉じた項になっている．構成要素に変項$t_\mathrm{1}$が含まれていることを考えると，$t_\mathrm{1}$は根に至るまでのどこかで束縛されているはずである．部分木をボトム・アップに観察することによって，その様子を調べてみよう．

### $\textit{Lucy loves } t_\mathrm{1}$

まず$\textit{Lucy loves } t_\mathrm{1}$という部分木を取り出すと，この部分の意味は既に知る通りFAとT&Pによって計算することができる．ただし，この項の意味計算には指標$1$の指示対象を定める必要がある．この要請はHaskellの型において`Lookup ctx 1 (ReaderEff PEANUTS)`という形で現れる．

```haskell
lucyLovesT1
    :: forall ctx
     . Lookup ctx 1 (ReaderEff PEANUTS)
    => Eff '[1 |-> PEANUTS] Bool
lucyLovesT1 = do
    t1      <- traceOrPronoun @1
    lovesT1 <- love t1
    Lucy & lovesT1
```

### $\textit{who}_\mathrm{1} \textit{ Lucy loves } t_\mathrm{1}$

次に，関係代名詞節$\textit{who}_\mathrm{1} \textit{ Lucy loves } t_\mathrm{1}$を見てみよう．この世の中の個体は，Lucyが愛するものとそうでないものに分類することができるので，$\llbracket \textit{who}_\mathrm{1} \textit{ Lucy loves } t_\mathrm{1} \rrbracket$は個体に関する述語である，つまり$\textit{who}_\mathrm{1} \textit{ Lucy loves } t_\mathrm{1}$は$e \to t$型をもつことが予想される．$\textit{loves}$の目的語である変項$t_\mathrm{1}$に様々な個体を代入してみては，それをLucyが愛してるか否かを検証する，といった具合である．

$\llbracket {\textit{who}_\mathrm{1} \textit{ Lucy loves } t_\mathrm{1}} \rrbracket$は純粋に個体に関する述語であり，${\textit{who}_\mathrm{1} \textit{ Lucy loves } t_\mathrm{1}}$という項の意味は変項割当に依存しないという点に注目したい．部分木である${\textit{Lucy loves t}_\mathrm{1}}$は意味計算にあたって$1$の指示対象が定まっている必要があるにも関わらずである．

このような観察から，この${\textit{who}_\mathrm{1}}$が併合されるこの接点において，$t_\mathrm{1}$の部分が抽象化され，$\lambda x. \text{Lucy loves } x$なる$D_e$から$D_t$への関数に変換されていることが見て取れる．実際のところ，このような意味計算はどのような規則に基づいて行われると解釈すべきであろうか．『形式意味論入門』4.4.に次のような言明がある．

> すると，wh句である${\textit{who}_\mathrm{1}}$が行っている「仕事」は何だろうか．統語論的には，${\textit{who}_\mathrm{1}}$は姉妹関係にある$t$タイプの接点を$\langle e, t \rangle$タイプに変換している．語彙的には，本来的にはT&Pによって$e$タイプの個体として外延を導かれるはずの痕跡$t_\mathrm{1}$を，変項に変換している．このふたつの操作を，同時に行うためには，${\textit{who}_\mathrm{1}}$の外延を固定して設定しFAによって併合を行うよりも，新たな意味解釈規則を設定するほうが，汎用性が高い．そこで使われるのがPAという規則である．
>
> つまり，関係代名詞${\textit{who}_\mathrm{1}}$は，「それ独自の外延をもたない」と考える．関係代名詞は，痕跡に割り振られている指標（index）と指示対象を同定する働きをしており，「単なる指標」として存在する．そして${\textit{who}_\mathrm{1}}$の行っている「仕事」は，語彙項目によってではなく，PAによって遂行される．

というわけで，いささか天下り的だが，第4の意味計算規則であるPredicate Abstractionを導入しよう．

Predicate Abstractionは以下のように定義される[^8]．

> $\alpha$が$\beta_i$ $(i \in \mathbb{N})$ および$\gamma$を娘に持つ枝分かれ接点で，$\beta$が代名詞または"such"であるとき，任意の変項割り当て$g$のもとで$\llbracket \alpha \rrbracket^g = \lambda x \in D_e. \llbracket \gamma \rrbracket^{g[i \mapsto x]}$となる．

ただし$g[i \mapsto x]$はmodified variable assignmentと呼ばれるもので，以下のように定義される[^9]．

> $g$を変項割当，$i \in \mathbb{N}$，$x \in D_e$とする．このとき，$g[i \mapsto x]$は以下の条件を満たす唯一の変項割当である．
>
> (i) $dom(g[i \mapsto x]) = dom(g) \cup {i}$
>
> (ii) $g[i \mapsto x](i) = x$
>
> (iii) $j \neq i$であるようなすべての$j \in dom(g[i \mapsto x])$について$g[i \mapsto x](j) = g(j)$

ここで関数$f$に対し$dom(f)$は$f$の定義域を表す．

非形式的には，元となる変項割当$g$に対し，$i$を与えたときに$x$を返すように局所的な改変を加えて得られる新たな変項割当が$g[i \mapsto x]$であると言うことができる．

PAを用いて$\textit{who}_\mathrm{1} \textit{ Lucy loves } t_\mathrm{1}$の意味計算を行ってみよう．

$$
\begin{align*}
\llbracket \textit{who}_\mathrm{1} \textit{ Lucy loves } t_\mathrm{1} \rrbracket
&= \lambda x. \llbracket \textit{Lucy loves } t_\mathrm{1} \rrbracket^{\phi[1 \mapsto x]} & \color{red}{(\because \text{PA})} \\
&= \lambda x. \llbracket \textit{love} \rrbracket^{\phi[1 \mapsto x]}(\llbracket t_\mathrm{1} \rrbracket^{\phi[1 \mapsto x]})(\llbracket \textit{Lucy} \rrbracket^{\phi[1 \mapsto x]}) & (\because \text{FA} \times 2) \\
&= \lambda x. (\lambda y z. z \text{ loves } y)(\llbracket t_\mathrm{1} \rrbracket^{\phi[1 \mapsto x]})(\text{Lucy}) & (\because \text{lexicon}) \\
&= \lambda x. (\lambda y z. z \text{ loves } y)(\phi[1 \mapsto x](1))(\text{Lucy}) & (\because \text{T\&P}) \\
&= \lambda x. (\lambda y z. z \text{ loves } y)(x)(\text{Lucy}) & (\because \text{definition of } \phi[1 \mapsto x]) \\
&= \lambda x. (\lambda z. z \text{ loves } x)(\text{Lucy}) & (\because \beta \text{-reduction}) \\
&= \lambda x. \text{Lucy loves } x & (\because \beta \text{-reduction})
\end{align*}
$$

PAをHaskellで実装すると以下のようになる．

```haskell
predicateAbstraction
    :: forall index entity ctx a
     . Eff ((index |-> entity) ': ctx) entity
    -> Eff ((index |-> entity) ': ctx) a
    -> (entity -> Eff ctx a)
predicateAbstraction _ x = runReaderEff @index x
```

`(index |-> entity) ': ctx`という部分に着目してみると，改変前の`ctx`という変項割当に対し，`index |-> entity`という対応関係をconsして上書きしており，modified variable assignmentを表現している．実装部分は単なる`runReaderEff`であり，まさにラムダ抽象を導入するというはたらきをしていることがわかる．

PAを用いた$\textit{who}_\mathrm{1} \textit{ Lucy loves } t_\mathrm{1}$の意味計算をHaskell上で行うと以下のようになる．

```haskell
-- | 与えられた個体はLucyによって愛されているか？
whoLucyLoves :: Eff ('[] :: [Assoc Nat (Type -> Type)]) (PEANUTS -> Bool)
whoLucyLoves = predicateAbstraction @1 who1 lucyLovesT1
  where
    who1 :: forall ctx. Lookup ctx 1 (ReaderEff PEANUTS) => Eff ctx PEANUTS
    who1 = traceOrPronoun
    lucyLovesT1 :: forall ctx. Lookup ctx 1 (ReaderEff PEANUTS) => Eff ctx Bool
    lucyLovesT1 = _
```

effectのlistが空（`'[]`）になっており，もはや発話文脈に対して何の制約も課されていないことがわかる．

### $\textit{boy } \textit{who}_\mathrm{1} \textit{ Lucy loves } t_\mathrm{1}$

$\textit{boy} : e \to t$および$\textit{who}_\mathrm{1} \textit{ Lucy loves } t_\mathrm{1} : e \to t$について，どちらも外延は個体に関する述語であるから，この部分木の意味計算はPMによって行う．

$$
\begin{align*}
\llbracket \textit{boy } \textit{who}_\mathrm{1} \textit{ Lucy loves } t_\mathrm{1} \rrbracket
&= \lambda x. \llbracket {\textit{boy}} \rrbracket(x) \land \llbracket {\textit{who}_\mathrm{1} \textit{ Lucy loves } t_\mathrm{1}} \rrbracket(x) & (\because \text{PM}) \\
&= \lambda x. x \text{ is a boy and Lucy loves } x
\end{align*}
$$

```haskell
-- | 与えられた個体は，少年であり，かつLucyによって愛されているか？
boyWhoLucyLoves :: Eff '[] (PEANUTS -> Bool)
boyWhoLucyLoves = predicateModification boy whoLucyLoves
  where
    boy, whoLucyLoves :: Eff '[] (PEANUTS -> Bool)
    boy = _
    whoLucyLoves = _
```

### $\textit{the boy } \textit{who}_\mathrm{1} \textit{ Lucy loves } t_\mathrm{1}$

定冠詞$the$の外延$\llbracket \textit{the} \rrbracket$はどのようなものだろうか．Heim and Kratzerでは以下のように定義されている[^10]．

> $f(x) = \text{True}$となるような$x$がちょうど1つだけ存在するような任意の関数$f \in D_{e \to t}$について，$\llbracket \textit{the} \rrbracket(f)$は，$f(x) = \text{True}$を満たすような唯一の$x$である．

与えられた述語$\varphi$を満たすような個体が，考えているモデル内にただ一つだけ存在するとき，その個体を$\textit{the } \varphi$の意味とする．Model PEANUTSにおける語彙の定義に立ち返ると，$\lambda x. \llbracket \textit{boy } \textit{who}_\mathrm{1} \textit{ Lucy loves } t_\mathrm{1} \rrbracket(x)$を$\text{True}$にする個体$x$は$\text{Schroeder}$のみなので，$\llbracket \textit{the boy } \textit{who}_\mathrm{1} \textit{ Lucy loves } t_\mathrm{1} \rrbracket = \text{Schroeder}$となるはずである．

Haskellでは以下のように実装できる[^11]．モデル内の個体を列挙する必要があるため，`Bounded entity`および`Enum entity`という制約を追加せざるを得ない．

```haskell
the
    :: (Bounded entity, Enum entity)
    => (entity -> Eff ctx Bool)
    -> Eff ctx entity
the predicate = do
    xs <- filterM predicate [minBound..maxBound]
    case xs of
        [x] -> pure x
        _   -> error "multiple entities satisfy the given predicate"
```

主部$\textit{the boy } \textit{who}_\mathrm{1} \textit{ Lucy loves } t_\mathrm{1}$の外延が$\text{Schroeder}$であることがわかったので，あとはFAによって$\llbracket \textit{player} \rrbracket(\text{Schroeder})$を計算すれば，元の文の外延が$\text{True}$であることがわかる．

```haskell
sentence :: Eff ('[] :: [Assoc Nat (Type -> Type)]) Bool
sentence = do
    let boyWhoLucyLoves = predicateModification boy whoLucyLoves
    theBoyWhoLucyLoves <- the boyWhoLucyLoves
    theBoyWhoLucyLoves & player
  where
    who1 :: forall ctx. Lookup ctx 1 (ReaderEff PEANUTS) => Eff ctx PEANUTS
    who1 = traceOrPronoun @1
    lucyLovesT1 :: forall ctx. Lookup ctx 1 (ReaderEff PEANUTS) => Eff ctx Bool
    lucyLovesT1 = do
        t1      <- traceOrPronoun @1
        lovesT1 <- love t1
        Lucy & lovesT1
    whoLucyLoves :: PEANUTS -> Eff ('[] :: [Assoc Nat (Type -> Type)]) Bool
    whoLucyLoves = predicateAbstraction @1 who1 lucyLovesT1

main :: IO ()
main = hspec $ do
    describe "The boy who(1) Lucy loves t(1) is a player" $ do

        it "is true" $ do
            let denotation = leaveEff sentence
            denotation `shouldBe` True
```

### §11のまとめ

- PAという意味計算規則が導入された
  - $\alpha$が$\beta_{i} (i \in \mathbb{N})$および$\gamma$を娘に持つ枝分かれ接点で，$\beta$が代名詞または"such"であるとき，任意の変項割り当て$g$のもとで$\llbracket \alpha \rrbracket^g = \lambda x \in D_e. \llbracket \gamma \rrbracket^{g[i \mapsto x]}$となる．
- PAは`runReaderEff`関数で表現できる

## 終わりに

本記事では『形式意味論入門』で紹介されている4つの意味計算規則を，その前提知識を交えて紹介し，Haskellのプログラムに書き下すことで理解を深めた．また，後半の2つの意味計算規則については，Haskell上で型レヴェルの制約を表現することで，発話文脈に関する型安全性を獲得することができた．

本記事では筆者の見切り発車と技術的な制約によりHaskellを採用したが，Haskellでは副作用を扱うにあたってモナドを明示的にユーザに見せるという点であまり良い選択肢ではなかったかもしれない．後編の内容に関して言えば，暗黙にKleisli圏の上で考えるような，algebraic effects and handlersをネイティヴに扱える言語の方が向いているかもしれないので，機会があれば挑戦してみたい．

『形式意味論入門』では専ら外延意味論が取り上げられているが，外延意味論では「$A$は$p$だと考えている」といった形式の，人間の信念に関する文を解釈することが難しい．このような問題に対するアプローチとして，内包意味論という枠組みが知られている．また，モデル論的意味論は，Tarskiによる数学の意味論に端を発するが，これに代わるものとして，証明論的意味論という意味論も展開されている．これはGerhard Gentzen[^12]やDag Prawitz[^13]らの数学の意味論を起源とし，PrawitzやMichael Dummett[^14]による，文の意味を検証条件とする立場を嚆矢とするものである．

「形式意味論の特徴はラムダ演算子である」という触れ込みだったが，実際に意味論の世界で用いられているのは標準的な集合論であり，ラムダ抽象および適用といった記法の使い勝手がよいので広く用いられている，というのが実態であるように感じられた[^15]．一方で，ラムダ計算の計算的側面に着目し，理論計算機科学の分野で知られている概念を輸入したり，型システムを拡充したりすることによって，有益な示唆が得られたり，実際にモデルの説明力が向上するかもしれない．例としては，量化やフォーカスといったいくつかの言語現象が，継続の概念を用いてうまく説明できることが報告されている[^16]．他には，依存型を用いると照応がうまく表現できると耳にしたことがある．また，Lambek計算およびコンビネータ論理に立脚した組み合わせ範疇文法（CCG; combinatory categorial grammar）も近年注目を集めているらしく，こちらも今後調べてみたい．

## コード全文

```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import "base" Control.Monad (filterM)
import "base" Data.Function ((&))
import "base" Data.Kind (Type)
import "base" Data.Proxy (Proxy (Proxy))
import "base" GHC.TypeNats (Nat)
import "extensible-skeleton" Data.Extensible.Effect (askEff, Eff, leaveEff, ReaderEff, runReaderEff)
import "hspec" Test.Hspec (describe, hspec, it, shouldBe)
import "membership" Type.Membership (type (>:), Assoc, Lookup)

data PEANUTS
    = Snoopy
    | Woodstock
    | Charlie
    | Sally
    | Lucy
    | Linus
    | Patty
    | Schroeder
    deriving (Bounded, Enum)

type (|->) :: Nat -> entity -> Assoc Nat (Type -> Type)
type (index :: Nat) |-> entity = index >: ReaderEff entity

boy :: PEANUTS -> Eff ctx Bool
boy x = pure $ case x of
    Charlie   -> True
    Linus     -> True
    Schroeder -> True
    _         -> False

love :: PEANUTS -> Eff ctx (PEANUTS -> Eff ctx Bool)
love object = do
    pure $ \subject -> pure $ case (subject, object) of
        (Lucy , Schroeder) -> True
        (Sally, Linus    ) -> True
        (Patty, Charlie  ) -> True
        _                  -> False

cute :: PEANUTS -> Eff ctx Bool
cute x = pure $ case x of
    Lucy  -> True
    Patty -> True
    Sally -> True
    _     -> False

dog :: PEANUTS -> Eff ctx Bool
dog x = pure $ case x of
    Snoopy -> True
    _      -> False

player :: PEANUTS -> Eff ctx Bool
player x = pure $ case x of
    Snoopy    -> True
    Charlie   -> True
    Lucy      -> True
    Linus     -> True
    Patty     -> True
    Schroeder -> True
    _         -> False

is_a :: a -> (a -> b) -> b
is_a x f = f x

the
    :: (Bounded entity, Enum entity)
    => (entity -> Eff ctx Bool)
    -> Eff ctx entity
the predicate = do
    xs <- filterM predicate [minBound..maxBound]
    case xs of
        [x] -> pure x
        _   -> error "multiple entities satisfy the given predicate"

predicateModification
    :: (a -> Eff ctx Bool)
    -> (a -> Eff ctx Bool)
    -> (a -> Eff ctx Bool)
predicateModification p q = \x -> do
    p' <- p x
    q' <- q x
    pure $ p' && q'

traceOrPronoun
    :: forall (index :: Nat)
     . forall (entity :: Type)
     . forall (ctx :: [Assoc Nat (Type -> Type)])
     . Lookup ctx index (ReaderEff entity)
    => Eff ctx entity
traceOrPronoun = askEff (Proxy @index)

predicateAbstraction
    :: forall index entity ctx a
     . Eff ((index |-> entity) ': ctx) entity
    -> Eff ((index |-> entity) ': ctx) a
    -> (entity -> Eff ctx a)
predicateAbstraction _ x = runReaderEff @index x

main :: IO ()
main = hspec $ do
    describe "Snoopy is a cute dog" $ do
        let cuteDog = predicateModification cute dog
            sentence = Snoopy `is_a` cuteDog

        it "is true" $ do
            let denotation = leaveEff sentence
            denotation `shouldBe` False

    describe "She(1) loves him(2)" $ do
        let sentence :: Eff '[1 |-> PEANUTS, 2 |-> PEANUTS] Bool
            sentence = do
                she      <- traceOrPronoun @1
                him      <- traceOrPronoun @2
                lovesHim <- love him
                she & lovesHim

        it "is true when she(1) refers Lucy and him(2) refers Schroeder" $ do
            let eval = leaveEff
                     . flip (runReaderEff @2) Schroeder
                     . flip (runReaderEff @1) Lucy
                denotation = eval sentence

            denotation `shouldBe` True

        it "is false when she(1) refers Lucy and him(2) refers Charlie" $ do
            let eval = leaveEff
                     . flip (runReaderEff @2) Charlie
                     . flip (runReaderEff @1) Lucy
                denotation = eval sentence

            denotation `shouldBe` False

    describe "The boy who(1) Lucy loves t(1) is a player" $ do
        let whoLucyLoves :: PEANUTS -> Eff ('[] :: [Assoc Nat (Type -> Type)]) Bool
            whoLucyLoves =
                let who1 :: Lookup ctx 1 (ReaderEff PEANUTS) => Eff ctx PEANUTS
                    who1 = traceOrPronoun @1
                    lucyLovesT1 :: Lookup ctx 1 (ReaderEff PEANUTS) => Eff ctx Bool
                    lucyLovesT1 = do
                        t1 <- traceOrPronoun @1
                        lovesT1 <- love t1
                        Lucy & lovesT1
                 in predicateAbstraction @1 who1 lucyLovesT1
            sentence = do
                let boyWhoLucyLoves = predicateModification boy whoLucyLoves
                theBoyWhoLucyLoves <- the boyWhoLucyLoves
                theBoyWhoLucyLoves `is_a` player

        it "is true" $ do
            let denotation = leaveEff sentence
            denotation `shouldBe` True
```

## 脚注

[^1]: 「自由変項とは何か」をまだ定義していないが，ここではラムダ計算などで現れる一般的な自由変数・束縛変数を想像してもらえば構わない．
[^2]: Heim and Kratzer §5.3.4
[^3]: ここで，これまでに導入した意味計算規則に対し，変項割当のもとでの意味計算を行えるような改変を加える必要があるが，自明な改変であるためここでは割愛する．詳しくはHeim and Kratzer §5.2.2を参照のこと．
[^4]: Heim and Kratzer §5.4.1．ただし原文では変項割当を$a$および$a'$で表しているが，$\alpha$と少し紛らわしいため$g$および$g'$を用いた．以下でも断りなく引用中でこのような記法の置き換えを行なうことがある．
[^5]: 変項でない終端記号を**定項**（constant） と呼ぶ．
[^6]: 詳しくはHeim and Kratzer §5.4.1を参照のこと．
[^7]: Heim and Kratzer §5.3.4
[^8]: Heim and Kratzer §5.5．ただし，$g[i \mapsto x]$は原文では$g^{x/i}$という表記になっている．
[^9]: Heim and Kratzer §5.3.4
[^10]: Heim and Kratzer §4.4.1
[^11]: 残念ながら型安全に実装する筋のいい方法を思い付かなかった．意欲のある読者は是非挑戦してほしい．
[^12]: ドイツの数学者・論理学者．自然演繹やシークエント計算などの功績で知られる．
[^13]: スウェーデンの哲学者・論理学者．自然演繹における証明の正規化可能性を示した業績などで知られる．
[^14]: イギリスの哲学者．Fregeの思想の研究や，論理の哲学・数学の哲学・言語哲学に対する貢献で知られる．
[^15]: Montagueによる原典にあたればまた印象が変わるのかもしれない．
[^16]: [Chris Barker, Continuations in Natural Language](https://www.cs.bham.ac.uk/~hxt/cw04/barker.pdf)
