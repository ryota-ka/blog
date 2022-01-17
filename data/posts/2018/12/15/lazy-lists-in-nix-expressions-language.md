# Nix Expression Language で遅延リストを作ってみる

この記事は [CAMPHOR- Advent Calendar 2018](https://advent.camph.net/) 15日目の記事です．14日目の記事は [@Rtm6Lgo](https://twitter.com/Rtm6Lgo) の [とある研究室の運営のエモいお話](https://rtm6.hatenablog.jp/entry/2018/12/14/113010) でした．

Nix Expression Language を用いて遅延リストを作る．普段は「ジェネレータを作ったり、遅延評価してみる」だが，Nix Expression Language は基本的に純粋なので，mutable な state をもつイテレータや，それを生成するジェネレータという概念はなじまない．

---

## これまでの流れ

- [Ruby の Enumerator でジェネレータを作ったり、遅延評価してみる - blog.ryota-ka.me](https://blog.ryota-ka.me/posts/2015/04/23/generators-and-lazy-evaluation-in-ruby)
- [Python でジェネレータを作ったり、遅延評価してみる – ymyzk’s blog](https://blog.ymyzk.com/2015/04/python-generator-lazy/)
- [ECMAScript 6 でジェネレータを作ったり、遅延評価してみる – ymyzk’s blog](https://blog.ymyzk.com/2015/04/ecmascript-6-generator-lazy/)
- [Rust でジェネレータを作ったり、遅延評価してみる - blog.ryota-ka.me](https://blog.ryota-ka.me/posts/2015/05/18/generators-and-lazy-evaluation-in-rust)
- [Swift でジェネレータを作ったり、遅延評価してみる – ymyzk’s blog](https://blog.ymyzk.com/2015/05/swift-generator-lazy/)
- [PHP でジェネレータを作ったり遅延評価してみる - たにしきんぐダム](https://tanishiking24.hatenablog.com/entry/2015/07/30/164111)
- [Scala でジェネレータを作ったり、遅延評価してみる - たにしきんぐダム](https://tanishiking24.hatenablog.com/entry/scala-generator)
- [Perl 6 でジェネレータを作ったり、遅延評価してみる - blog.ryota-ka.me](https://blog.ryota-ka.me/posts/2015/12/30/generators-and-lazy-evaluation-in-perl-6)
- [Vim script でジェネレータを作ったり、遅延評価してみる - blog.ryota-ka.me](https://blog.ryota-ka.me/posts/2016/12/08/generators-and-lazy-evaluation-in-vim-script)

## Nix とは

純粋関数型パケッジマネジャー．[公式サイト](https://nixos.org/nix/)では以下のように紹介されている．

> Nix is a powerful package manager for Linux and other Unix systems that makes package management reliable and reproducible. It provides atomic upgrades and rollbacks, side-by-side installation of multiple versions of a package, multi-user package management and easy setup of build environments.

## Nix Expression Language とは

[Homebrew](https://brew.sh/) では Ruby が使われるが，Nix では Nix Expression Language が用いられる．例えば [GNU Hello](https://www.gnu.org/software/hello/) の derivation は以下のように記述される[^1]．

```nix
{ stdenv, fetchurl }:

stdenv.mkDerivation rec {
  name = "hello-${version}";
  version = "2.10";

  src = fetchurl {
    url = "mirror://gnu/hello/${name}.tar.gz";
    sha256 = "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i";
  };

  doCheck = true;

  meta = with stdenv.lib; {
    description = "A program that produces a familiar, friendly greeting";
    longDescription = ''
      GNU Hello is a program that prints "Hello, world!" when you run it.
      It is fully customizable.
    '';
    homepage = https://www.gnu.org/software/hello/manual/;
    license = licenses.gpl3Plus;
    maintainers = [ maintainers.eelco ];
    platforms = platforms.all;
  };
}
```

以下混同の恐れが無い限り，Nix Expression Language やその処理系を指して Nix と呼ぶ場合がある．

基本的な文法については雰囲気で読めると思われるので，以下ではこの記事を読むにあたって必要な部分の解説のみ行う．

### lists

list はいくつかの値を順に並べたもので，square bracket を用いて `[ 42 "Hello" ]` のように書かれる．list はそれぞれの値については lazy であるが，list の長さについては strict である．なので，無限の長さを持つような list を作ることはできない．

```nix
nix-repl> let xs = [ 42 ] ++ xs; in xs
error: infinite recursion encountered, at (string):1:20
```

### sets

set は名前と値の組をいくつか集めてきたもので，Python の辞書，JavaScript のオブジェクト，Ruby のハッシュに相当する[^2]．それぞれの組は "attribute" と呼ばれる．

```nix
nix-repl> person = {
            name = "ryota-ka";
            age = 25;
          }

nix-repl> person.name
"ryota-ka"
```

また，各 attribute は lazy に評価される．

```nix
nix-repl> person = {
            name = "a lady";
            age = abort "ouch";
          }

nix-repl> person.name
"a lady"
```

set の先頭に `rec` というキーワードを付けると，set 内の attribute を再帰的に参照できるようになる．

```nix
nix-repl> { name = "lib-${version}"; version = "0.1.0"; }
error: undefined variable 'version' at (string):1:17

nix-repl> rec { name = "lib-${version}"; version = "0.1.0"; }
{ name = "lib-0.1.0"; version = "0.1.0"; }
```

### 関数

`pattern: body` という形で書かれるものは関数である．`pattern` と書いたが，本記事では単一の識別子を用いたパターンマッチ以外は登場しないので，`arg: body` と読み替えてもらっても構わない．

```nix
nix-repl> double = x: x * 2

nix-repl> double 42
84
```

`f x` は適用である．念のため．

## 遅延リストを作る

とりあえず遅延リストを作っていく．遅延リストは先頭の値 `hd` と後続の遅延リスト `tl` との組 `(hd, tl)` で表すことにする．ただし，列が打ち止めになる場合，`tl` に `null` を入れておくものとする．Nix にタプルは存在しないので，組を表現するために set を使うことにする．

以下では無限に1が続く列を定義している．

```nix
nix-repl> let
            ones = {
              hd = 1;
              tl = ones;
            };
          in
            ones
{ hd = 1; tl = { ... }; }
```

また，フィボナッチ数列は以下のように定義できる．

```nix
nix-repl> let
            fib =
              let
                aux = a: b: { hd = a; tl = aux b (a + b); };
              in
                aux 0 1;
          in
            fib
{ hd = 0; tl = { ... }; }
```

## `toList` 関数

このままだと取り扱いに困るので，遅延リストを Nix の list に変換できるようにしたい．これを実現する `toList` 関数を以下のように再帰的に定める．

```nix
nix-repl> let
            ones = { hd = 1; tl = ones; };
            toList = xs: [ xs.hd ] ++ (if xs.tl == null then [] else toList xs.tl);
          in
            toList ones
zsh: segmentation fault  nix repl
```

`ones` は無限の長さを持つのでセグメンテーション違反が発生してしまった．

## `take` 関数

遅延リストの先頭のいくつかの要素だけを取り出す `take` 関数を実装する．

```nix
nix-repl> let
            fib = let aux = a: b: { hd = a; tl = aux b (a + b); }; in aux 0 1;
            toList = xs: [ xs.hd ] ++ (if xs.tl == null then [] else toList xs.tl);
            take = n: xs:
              if n == 0
              then null
              else { hd = xs.hd; tl = take (n - 1) xs.tl; };
          in
            toList (take 10 fib)
[ 0 1 1 2 3 5 8 13 21 34 ]
```

## 関数をファイルに定義する

定義すべきものが増えて，すべてを REPL に打ち込むのが大変になってきたので，汎用的な関数はファイルに記述していくことにする．`lazy-lists.nix` というファイルを用意して，以下のように書いておく．

```nix
rec {
  toList = xs: [ xs.hd ] ++ (if xs.tl == null then [] else toList xs.tl);
  take = n: xs:
    if n == 0
    then null
    else { hd = xs.hd; tl = take (n - 1) xs.tl; };
}
```

REPL 上で `:l` コマンドを使うと，この set の attributes が変数として展開され，利用できるようになる．

```nix
nix-repl> :l lazy-lists.nix
Added 2 variables.

nix-repl> toList
«lambda @ /Users/ryota-ka/lazy-lists.nix:2:12»

nix-repl> take
«lambda @ /Users/ryota-ka/lazy-lists.nix:3:10»
```

以降，遅延リストを扱う関数はここに追記していくものとする．

## `map` 関数・`filter` 関数

`map` と `filter` くらいがあると安心して年を越せそうなので，定義しておく．

```nix
map = f: xs:
  {
    hd = f xs.hd;
    tl = map f xs.tl;
  };
```

```nix
filter = f: xs:
  let
    tl = filter f xs.tl;
  in
    if f xs.hd
    then { hd = xs.hd; tl = tl; }
    else tl;
```

`tl = tl` と書いた部分は，Nix らしく `inherit tl` と書くこともできる．

## いつものやつ

フィボナッチ数列をそれぞれ2乗し，奇数のもののみ抜き出し，先頭から10個の要素を取り出してリストにしたものがこちら．

```nix
nix-repl> :l <nixpkgs>
Added 9706 variables.

nix-repl> :l fib.nix
Added 1 variables.

nix-repl> :l lazy-lists.nix
Added 4 variables.

nix-repl> let
            sq = n: n * n;
            odd = n: builtins.bitAnd 1 n != 0;
          in
            toList (take 10 (filter odd (map sq fib)))
[ 1 1 9 25 169 441 3025 7921 54289 142129 ]
```

## 過去のもの

[](https://ryota-ka.hatenablog.com/entry/2015/12/30/000000)

[](https://ryota-ka.hatenablog.com/entry/2016/12/08/000000)

## おわりに

CAMPHOR- Advent Calendar 2018，明日は [@tomokortn](https://tomokortn.myportfolio.com/) の記事です．お楽しみに！

## 脚注

[^1]: https://github.com/NixOS/nixpkgs/blob/703827f36cc65b137eed4d759b31827d29733072/pkgs/applications/misc/hello/default.nix より引用．
[^2]: https://nixos.wiki/wiki/Nix_Expression_Language による．
