---
keywords:
  - Bertrand Russell
  - Gottlob Frege
  - Lambda calculus
  - Logicism
  - Ludwig Wittgenstein
  - Russell's paradox
---

# Russell のパラドクスと λx.xx —— または自己言及がもたらす豊かさと危うさについて

お盆に数日の休みがあったので，Ludwig Wittgenstein の『論理哲学論考 (Tractatus Logico-Philosophicus)』を読み始めた．Wittgenstein の著作を読んでいると，彼が理論を継承し，また批判する対象となった Gottlob Frege や Bertrand Russell の思想にも同時に触れることになる．

『論考』の命題3.3以下に，Russell のパラドクスに対する解決策を提示し，Russell の主張を反駁する場面が見られるが，この記事ではむしろ，Russell 自身の回答である**階型理論** (theory of types) による解決策に目を向けたい．我々が普段プログラムを記述する際に触れる型の概念が如何にして生まれ，発展してきたのかを見ていくこととしよう．

---

## Russell のパラドクス

既にご存知の読者も多いと思うが，念のため Russell のパラドクスについての紹介をしておく．

> $R = \lbrace x \mid x \notin x \rbrace$ なる集合を考える．つまり $R$ は，**自分自身を要素として含まない集合全体の集合**である．まず，$R \in R$ と仮定すると，$R$ の定義より $R \notin R$ であるはずなので矛盾する．一方，$R \notin R$ と仮定すると，$R$ の定義より $R \in R$ となるはずなので，こちらも矛盾する．

## パラドクスの発見

Frege の著書を読み，パラドクスを発見した Russell は，1902年に Frege へ書簡[^1]を送付した．以下はその内容の一部である．

> Let $w$ be the predicate: to be a predicate that cannot be predicated of itself. Can $w$ be predicated of itself? From each answer its opposite follows. Therefore we must conclude that $w$ is not a predicate.
>
> (抄訳) $w$ を「自分自身に述語付けられない述語」だとする．$w$ は自分自身に述語付けられるだろうか？どちらの答えからもその反対が帰結する．それゆえ，$w$ は述語ではないと結論せざるを得ない．

書簡では上記の部分に続き，集合論に置き換えた場合の解説がされている．後ほど紹介する包括原理さえ公理として採用すれば立ちどころに矛盾が生じるので，当時は論理の矛盾として深刻に受け止められたようである[^2]．

この書簡を受け，当の Frege は彼の著作『算術の基本法則 (Grundgesetze der Arithmetik)』第2巻の補遺に以下のような痛切なコメントを残している[^3]．

> Einem wissenschaftlichen Schriftsteller kann kaum etwas Unerwünschteres begegnen, als dass ihm nach Vollendung einer Arbeit eine der Grundlagen seines Baues erschüttert wird.
> In diese Lage wurde ich durch einen Brief des Herrn Bertrand Russell versetzt, als der Druck dieses Bandes sich seinem Ende näherte.
>
> (抄訳) 学術的な著作を著す者にとって，自らの仕事を完遂した後に，その建築を基礎から揺るがされる事態に遭遇することほど望ましからぬことはない．私が Bertrand Russell 氏からの手紙によってこのような状況に追いやられたのは，この巻の出版が終わろうとしていた頃のことだった．

文字通り Grundgesetze (en: constitutions / basic laws) を築こうとした数学者が，自らが定めた公理系から矛盾が導かれることを知った際の心情は推し量るに極めて忍びないが，ともかく彼の第5法則 (Grundgesetz V) はこのような災厄を引き起こしてしまった．

## Frege の第5法則 —— 概念と外延

Frege が築き上げた緻密な体系にヒビを入れることとなってしまった第5法則を見てみることとしよう．『基本法則』第1巻§10で述べられている主張を現代的な記法を交えて書き直すと[^4]以下のようになる．

$$
\acute{\varepsilon}\phi(\varepsilon) = \acute{\varepsilon}\psi(\varepsilon) \Leftrightarrow \forall x(\phi(x) = \psi(x))
$$

ここで $\acute{\varepsilon}\phi(\varepsilon)$ という記法に関する説明が必要だが，Frege は各命題関数 $\phi$ に対する値域 (_Werthverlauf_) という概念をそのように記述する．伝統的論理学において，ある概念 (Begriff) が当てはまる範囲を**外延** (Umfang) と呼ぶが，$\phi$ の値域は，概念 $\phi$ を満たすようなものの集まりなので，外延のことだと思っていいだろう．Frege のこのような態度は，記号 (Zeichen) について論じる『基本法則』第2巻§99からも読み取れる．

> Was man durch Abstrahiren vielleicht erreichen kann, ist ein Begriff, und wenn wir den Umfang eines Begriffes kurz „Klasse“ nennen, so können wir alle gleichgestalteten Zeichen zu derselben Klasse rechnen.
>
> (抄訳) 抽象化を通じて手にすることができる可能性があるものは概念であり，その概念の外延を「クラス」と付けるならば，我々は皆同じ形をした記号を同じクラスの中に認めることができる．

ともかく，$\acute{\varepsilon}\phi(\varepsilon)$ と書けば，それは $\phi$ を満たすものを集めてきたクラス[^5]ということになる．なるのだが，一体「$\phi$ を満たすもの」はどこから集めてくるのだろうか？

## 包括原理

素朴集合論 (naïve set theory) のもとでは，通常以下の**包括原理** (comprehension principle) を認める[^6]．

> 任意の述語 $\varphi(x)$ について，その述語を満たす元を集めた集合 $\lbrace x \mid \varphi(x) \rbrace$ が存在する．

どのような述語を選んだとしても，それを満たすような要素すべてをどこからともなく集めてきた集合を作ることができる——現代に生きる我々からすれば，これはかなり強い，ともすれば無茶な仮定に見えるものだが，20世紀初頭まで人々はこのような仮定を無邪気に信じていた．前節で見たように，Frege さえもそうであった．恐らく伝統的論理学における「概念」とそれに対応する「外延」という思考様式が支配的であった時代においては，包括原理は至極当然の要求だったのではないだろうか．しかし，このような主張を公理として認めることで，冒頭で述べた Russell のパラドクスが生じ，矛盾を招き入れてしまうことは現在広く知られる通りである．

ところで，体系に矛盾が生じると何が問題になるのだろうか．通常の論理学の枠組みでは，矛盾 $\bot$ からは任意の命題が帰結するのであった[^7]．記号で書けば，任意の命題 $P$ について，$\bot \supset P$ が真だと言えてしまう．つまり，ある体系の中でどれだけ実りある議論が展開できたとしても，その体系がひとたび矛盾を含めば，体系内の任意の命題が証明できてしまうことになり，せっかくの議論の意義はすべて無に帰してしまうのである[^8]．

さて，包括原理を公理として採用することにより，Russell のパラドクスが生じることを見たが，これを解消するためには以下のいずれかを迫られることになる．

- 包括原理を認めない，または制限を加える
- 包括原理を認めも矛盾が生じない論理体系を採用する

前者は素直なアプローチであり，現代数学における主流な解決策は言わずもがなこちらである．Russell のパラドクスを独立に発見していたとされる Ernst Zelmero が1908年に提出した公理系を改良したものである ZF 公理系は現在の数学の基礎となっている．

一方後者は，Russell のパラドクスが古典論理から導かれる[^9]ことを逆手に取ったもので，N. V. Grišin が古典論理から縮約規則[^10]を取り除いた論理体系の無矛盾性を示している[^11]．線形論理やアフィン論理も縮約規則を前提しないので，もしかすると包括原理を採用しても矛盾が生じないかもしれない．残念ながらここまでは調べが付いていない．

## Russell の階型理論

Russell は自らが指摘したこのパラドクスの解消のために奔走する．彼が採ったのは包括原理に制限を加えるアプローチだった．当時は集合という概念自体がはっきりと定義づけられたものではなく，それ故にパラドクシカルな集合を作り出すことができてしまっていたため，彼はそこにいくらかの制限を加えようと試みたのである．

Russell 自身，$x \in x$ なる表現が，集合とその要素を区別しないことについては早くから問題視していたようで，件の書簡を送付した翌年，1903年に出版された『数学の諸原理 (Principles of Mathematics) 』の補遺 "The doctrine of types" において既に，「"$x$ is an $x$" などという表現は無意味であるから，"$x$ is a $u$" なる表現においては $x$ と $u$ は異なる型を持たねばならない」と記している[^12]．その後紆余曲折あったようだが，1908年には**分岐タイプ理論** (ramified type theory) を発表する[^13]．

実は階型理論は Russell のパラドクスのみを解消するためだけの道具立てというわけではなく，当時知られていたいくつかのパラドクスを同時に片付けてしまおうという試みであった．実際，論文中では Epimenides のパラドクス[^14]や，Burali-Forti のパラドクス[^15]など，いくつかの例が挙げられている．Russell はこれらの誤謬は**悪循環原理** (_vicious-circle principle_) によって解決されると主張する．悪循環原理とは，「いかなる『〜をすべて集めてきたもの』もそれ自身に基づいて定義される要素を含んではならない[^16]」というもので，この原則を専門用語で言い換えた「束縛変数を含む命題や関数は何であれ，それ自身を引数として入力してはいけない」という制約を敷く．この悪循環原理を形式的に遂行するため，命題関数の量化を反映した階層構造を設定した上で，項がどの階層に属するを分類し，命題や関数の引数として出現できる項の種類に制限を加えるのが階型理論の目的である．

抽象的な説明が続いてしまったので，具体例を通じて階型理論のエッセンスを見てみよう．階型の分類は以下のように行われる．

- 命題でも関数でもない対象を individuals と呼ぶ
  - これは0階とみなされる
- 引数に individual のみが出現する関数を1階の関数と呼ぶ
- ある関数において，引数または束縛変数として n 階の変数が最高位の変数として出現するならば，その関数は n+1 階である

この規則に当てはめると，例えば $\text{Socrates}$ や $\text{Plato}$ という対象は命題でも関数でもないので0階の項であり，$\phi(X) = X \text{ is mortal}$ や $\psi(X) = X \text{ is a philosopher}$ といった命題関数は，引数として0階のものを取るので1階の項である．このとき，$\phi(\text{Socrates})$ や $\psi(\text{Plato})$ などと言うことはできるが，$\phi(\psi)$ と言うことはできない．$\psi$ は1階の項であるため，これを引数として取る $\phi$ は2階以上でなければならないためである．

このような階層構造を導入することにより，Russell は「$x$ は $x$ の要素である」のような記述はそもそも項として存在し得ず，無意味であると主張する．かくして Russell のパラドクスは片付く[^17][^18]．

## 型無しラムダ計算

20年ほど時代を下り，1930年代に話題を移そう．若き日の Alonzo Church は，自由変数を用いない[^19]形式論理学の記法あるいは計算体系として，ラムダ計算を提案する．初出は1932年の [A Set of Postulates for the Foundation of Logic](https://www.jstor.org/stable/1968337) であるようで，表題からも分かる通り，この頃の Church はラムダ計算を論理学の基礎として据えようと考えていたらしく，項として種々の論理定項を含んでいる．しかし，このオリジナルの体系は証明力が強すぎたため，後に Stephen Kleene と John Barkley Rosser らにより矛盾を導くことが証明された[^20]．

## Curry のパラドクス

Church のオリジナルの体系の矛盾を導くために，Curry のパラドクス[^21]というものが知られているので紹介する．

任意の命題 $y$ について $r := \lambda x. (x x \supset y)$ とおく．このとき，

$$
\begin{align*}
rr &= (\lambda x. (x x \supset y))(\lambda x. (x x \supset y) & \because \text{definition of } r \\
 &= (\lambda x. (x x \supset y))(\lambda x. (x x \supset y)) \supset y & \because \beta \text{-reduction} \\
 &= rr \supset y & \because \text{definition of } r
\end{align*}
$$

ここで $rr$ が偽であると仮定すると，ex falso quodlibet より $rr \supset y$ は真となるが，これと等価であるはずの $rr$ は仮定より偽なので矛盾を導く．よって，$rr$ は真であり，これと等価である $rr \supset y$ もまた真となるので，modus ponens より $y$ も真である．このようにして，任意の命題 $y$ の証明が可能となってしまう．

余談にはなるが，Curry のパラドクスはいくつかの形が知られている．以下はラムダ計算の代わりに命題論理を用いたものである．

命題 $X$ を $X := X \supset Y$ と定義する．これは「この文が真ならば Y は真である」と読める．このとき，

$$
\begin{align*}
& \vdash X \supset X & \because \text{tautology} \\
(X = X \supset Y) \land (X \supset X) & \vdash X \supset (X \supset Y) & \\
& \vdash X \supset Y & \\
& = X & \because \text{definition of } X \\
X \land (X \supset Y) & \vdash Y & \because \text{modus ponens}
\end{align*}
$$

となり，任意の命題 $Y$ が帰結される．また，異なるバリエーションとして，$Y = \bot$ とおくと，$(X \supset (X \supset Y)) \vdash (X \supset (X \supset \bot)) \vdash (X \supset \neg X)$ となり，これは「この文は偽である」と主張する Epimenides のパラドクスに他ならない．

種々のパラドクスの発見により，ラムダ計算を論理学の基礎として据える Church の計画は頓挫することとなったようである．しかしながら，計算模型としての側面だけを取り出しても非常に豊かな内容を含んでいたので，以降は主にそのような側面から考察されこととなる．

## 単純型付ラムダ計算

1940年に発表された [A Formulation of the Simple Theory of Types](https://www.jstor.org/stable/2266170) という論文が，型付きラムダ計算の初出とされている．ラムダ計算と階型理論を統合することで，どのような嬉しい性質が生じるのかはこの論文には記されていないが，ともかく Church は従来のラムダ計算に加えて，型という概念を導入する．

- $\iota$ を individuals の型，$\omicron$ を真偽値の型とする
- $\alpha$ と $\beta$ が型であるとき，$\alpha\beta$ は型である

現在の一般的な単純型付ラムダ計算[^22]との差異は以下である．第一に，現代では**基本型** (basic types, ground types) の集合 $\sigma, \tau, \ldots$ が既に定まっているものとして議論を始めが，Church は $\iota$ と $\omicron$ のみを考える[^23]．第二に，関数の型は現代では普通 $\beta \to \alpha$ と書かれるが，Church は $\alpha\beta$ と書く．

Church はまず以下の**原子記号** (primitive symbols) を導入する．

$$
\lambda, (, ), N_{oo}, A_{ooo}, \Pi_{o(o \alpha)}, \iota_{\alpha(o \alpha)}, a_{\alpha}, b_{\alpha}, \ldots, z_{\alpha}, \bar{a_{\alpha}}, \bar{b_{\alpha}}, \ldots .
$$

有限の原始記号列を**式** (formula) と呼ぶのだが，ある種の式を**整式** (well-formed formula) として区別し[^24]，それらはそれぞれ型を持つとする．整式の型は以下の規則に従って決定される．

1. 単一の原子記号 (ただし $\lambda, (, )$ を除く) からなる式は整式であり，下付き文字で書かれた型をもつ．
2. $x_{\beta}$ が変数であり，$M_{\alpha}$ が整式であるとき，$\lambda x_{\beta} M_{\alpha}$ は型 $\alpha\beta$ をもつ整式である．
3. $F_{\alpha \beta}$ と $A_{\beta}$ が整式で，それぞれ型 $\alpha \beta$ と $\beta$ を持つとき，$F_{\alpha \beta}A_{\beta}$ は型 $\alpha$ をもつ整式である．

さて，このような型付け規則を伴った計算体系を築くことにより，一体どのような恩恵が得られるのだろうか．

型無しラムダ計算においては，無限の長さのβ簡約列を持つ項が存在する．例えば $(\lambda x.xx)(\lambda x.xx) \to_{\beta} (\lambda x.xx)(\lambda x.xx) \to_{\beta} \ldots$ などが考えられる．しかしながら，単純型付ラムダ計算においては，上記のような項は一級市民として扱われない．$\lambda x. xx$ という項は型付けすることができず，式ではあるものの整式としては扱われないためである．

一般に $M$ をラムダ計算の式として，$M$ から始まる無限の長さのβ簡約列が存在しないとき，$M$ は**強正規化可能** (strongly normalizable) というが，単純型付きラムダ計算の任意の整式は強正規化可能であることが知られており，この事実を**強正規化定理** (strong normalization theorem) と呼ぶ．また，Church-Rosser 性により，その正規形は一意に定まる．この項書き換え系を計算ステップと見たとき，強正規化定理は計算の停止性に対応する．

$\lambda x.xx$ は見るからにきな臭い[^25]項であり，実際に Curry のパラドクスのような災厄をしばしばもたらすのであるが，我々がこの項から本能的に感じ取る危険性は，恐らく関数とその引数を区別していない点である．それはちょうど，集合とその要素を区別せず，$x \notin x$ などという記述をしてしまったがために Russell のパラドクスが引き起こされたのとよく似ている．しかし，単純型付ラムダ計算では，$\lambda x. x x$ なる項は型付け不可能として退けられる[^26]．Russell が型の概念を導入したことにより，素朴集合論に生じた矛盾を回避したのと同様に，Church も型無しラムダ計算で生じた矛盾を回避し，おまけに任意の項が一意な正規形をもつという綺麗な性質まで手にすることができた．

## 停止性問題・Gödel の不完全性定理・対角線論法

Russell のパラドクスに始まり，$x \in x$ や $\lambda x.xx$ といった自己言及・自己適用を含んだ表現について見てきたが，計算機科学の素養がある読者なら，停止性問題の決定不能性の証明が，ある種の自己適用を用いたものだったことを思い出すかもしれない[^27]．Gödel の不完全性定理に登場する「この文は証明できない」といういわゆる Gödel 文も自己言及を含んでいる．このように，ある種の自己言及を仮定すると矛盾を導く論法は対角線論法として知られている．

## 終わりに

これまで見てきたように，制限のない自己言及はしばしば矛盾を引き起こす．これを防ぐために Russell は型の概念を発明し，論理学における式がどのような文脈で出現できるかに制限を加え，Church はこのアイディアを拝借し，ラムダ計算における式がどのような文脈で出現できるかに制限を加えた．

ラムダ計算に対して型の概念を導入したことは，停止性を獲得し，任意の項に対して一意な正規形の存在を保証する結果となったが，これは裏を返せばチューリング完全性を失ったということである．停止性の保証されない型無しの体系は自由であるが危うく，停止性が保証される型付きの体系は息苦しくも安全である．Haskell などは型付きラムダ計算をベースとしつつも，再帰関数を記述できる能力を再び与えられているし，実際に我々が普段書くプログラムのほとんどはチューリング完全なのであるが，最近では [Dhall](https://github.com/dhall-lang/dhall-lang) や [bosatsu](https://github.com/johnynek/bosatsu) など，チューリング完全でないが故に停止性が保証できることを特長として謳う言語も登場している．

Russell の型理論も Church のラムダ計算も，論理学，ひいては数学の基礎付けを与えるという願いを成就することはできなかった．しかし，これらが組み合わさった際に，プログラムと証明との間に対応関係が浮かび上がり，再び論理学の世界へと通じる道が開くことは Curry-Howard 同型対応としてよく知られている．結局のところ，両者の思想ともに，人間の思考の根源的な営みである論理や推論と言った概念にどこか根ざしているように思われて仕方ないのである．

## 脚注

[^1]: [Bertrand Russell, Letter to Frege, 1902](http://www.thatmarcusfamily.org/philosophy/Course_Websites/Readings/Russell%20-%20Letter%20to%20Frege.pdf)
[^2]: [古森雄一, 汎用システムとしての 「ラムダ計算 + 論理」, 2007](http://www.kurims.kyoto-u.ac.jp/~kyodo/kokyuroku/contents/pdf/1533-5.pdf)
[^3]: [Gottlob Frege, Grundgesetze der Arithmetik, Band II, S.253](<https://glossar.hs-augsburg.de/Frege,_G._(1903):_Grundgesetze_der_Arithmetik,_Band_II>)
[^4]: Frege の用いた記法は，現代数学において用いられる記号とは幾分異なっており，中には記述することが容易でないものも含まれるため，このような妥協を余儀なくされている．余談ではあるが，論理主義の黎明期において，論理学が扱う対象を名辞から記号へと転換し，論理を記号として扱うための体系を整備したこともまた Frege の偉大な仕事の一つとして見做されている．興味のある読者は『概念記法 (Begriffsschrift)』を参照のこと．
[^5]: 当時は集合のことを単にクラス (_en: class / de: Klass_) と呼んでいることに注意．
[^6]: そもそも確立された定義がないことが素朴集合論を特徴付けているので，人々の心の中に「私が考える素朴集合論」があってよいのだが，ここはどうかひとつ包括原理を認めていただきたい．
[^7]: "ex falso quodlibet" として知られる．
[^8]: このままいくと，冒頭の導入に反して，この記事で Wittgenstein の思想に触れる箇所が皆無になってしまいそうなので，脚註という形で補論を付けておく．Wittgenstein 自身は矛盾に対していくらか冷淡な態度をとっている．彼は『論考』命題5.12においてこう述べている．"Insbesondere folgt die Wahrheit eines Satzes »p« aus der Wahrheit eines anderen »q«, wenn alle Wahrheitsgründe des zweiten Wahrheitsgründe des ersten sind." (訳: 特に，命題 $q$ の真理根拠すべてが命題 $p$ の真理根拠に含まれるとき，$q$ が真であることから $p$ が真であることが帰結する．) つまり，Wittgenstein が考える含意関係 $q \supset p$ の元では，$q$ が真である際にのみ $p$ が真であることが帰結するのである．Wittgenstein 自身も「矛盾こそが最も多くのことを語る」命題だと気付き，いくらかの当惑を感じていたようだが，このような制約を付け加えることによって問題を回避しようとした．
[^9]: 直観主義論理を採用しても同様の問題は生じる．
[^10]: 同じ仮定は何度用いてもよいとする規則．縮約規則を採用しない場合，仮定は推論の過程で「消費」されることになる．
[^11]: [V. N. Grišin, Predicate and set-theoretic calculi based on logic without contractions, 1982](http://www.mathnet.ru/links/468f90803fffa29e2c1aba7e2b5ddd4d/im1547.pdf)
[^12]: [Bertrand Russell, The Principles of Mathematics, Appendix B. The Doctrine of Types, §497, 1903](http://fair-use.org/bertrand-russell/the-principles-of-mathematics/appendix-b)
[^13]: [Bertrand Russell, Mathematical Logic as based on the Theory of Types, 1908](https://www.jstor.org/stable/pdf/2369948.pdf)
[^14]: クレタ人である Epimenides は「クレタ人は皆嘘つきである」と言った．
[^15]: 「すべての順序数全体の集合」を考えたときに，それ自身も順序数としての性質を満たしてしまう．
[^16]: 原文では "no totality can contain members defined in terms of itself"
[^17]: Hiermit erledigt sich Russells Paradox. (Tractatus 3.333)
[^18]: Russell のパラドクスの発見により，Frege の論理主義の夢は潰えたのであるが，分岐タイプ理論による修正を経て，Russell は Alfred North Whitehead との共著『プリンキピア・マテマティカ (Principia Mathematica)』において分岐タイプ理論をベースにした数学の基礎付けを展開する．しかしながら，そのような理論修正を以てしても，その後論理主義が大きく花開いたわけではなかった．論理学から純粋に導出できないいくつかの原理を公理として認めざるを得なかったからである．(そのうち階型理論と関連が深いものとしては還元公理 (axiom of reducibility) が知られている．) 結局のところ『プリンキピア』は，論理学を用いて数学の体系を基礎付けるという画期的な試みでありつつも，同時に論理主義の限界をも示すこととなったのである．
[^19]: もちろんラムダ式自体には自由変数は出現するのであるが，ラムダ計算の体系から証明可能な式は自由変数を含まない．
[^20]: [S. C. Kleene and J. B. Rosser, The Inconsistency of Certain Formal Logics, 1935](https://www.jstor.org/stable/1968646)
[^21]: 名前は論理学者 Haskell Curry に由来する．
[^22]: ここでは横内寛文『プログラム意味論』 (共立出版) の定義を参考にした．
[^23]: おもしろいことに，これは自然言語の形式意味論を考える際に与える意味タイプの定義と一致している．ただし，形式意味論では通常 individual ではなく entity と呼ぶ．
[^24]: 「整式」の概念自体は1932年の論文中でも登場するが，型の概念が導入されたことにより，型付け可能な項のみに制限されることとなった．
[^25]: 「きな臭い」という人間の直観を根拠にした表現を用いたが，本来ラムダ式が策定するのは統語論の範疇のみであり，意味論とは切り離されて考えられるべきであることに注意されたい．
[^26]: ちなみに，二階ラムダ計算としても知られる System F においては，$x : \forall X. X \to X$ とすれば $x x$ が型付け可能となる．この計算体型では項だけでなく型の量化も許される．
[^27]: 私は計算機科学の素養がないので，同僚との会話の中でなされた指摘によって気付いた．
