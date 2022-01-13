# Perl 6 でジェネレータを作ったり、遅延評価してみる

この記事は [CAMPHOR- Advent Calendar 2015](http://advent.camph.net/) 30日目の記事です．

## はじめに

今月2015年12月は，[12月3日の PHP 7 のリリース](http://php.net/archive/2015.php#id2015-12-03-1)に始まり，クリスマスには [Ruby 2.3](https://www.ruby-lang.org/en/news/2015/12/25/ruby-2-3-0-released/) 並びに [Perl 6](https://perl6advent.wordpress.com/2015/12/24/an-unexpectedly-long-expected-party/) のリリースが相次いだため，往年のスクリプト言語たちにとっては華々しいひと月となった．調べてみたところ，PHP 5.0.0 のリリースは2004年1月，Perl 5.000 のリリースは1994年10月だったそうなので，やはり大型のリリースが相次いだのだなという気分になる．

というわけでこの際なので，最近ではもはや CAMPHOR- の伝統芸能となりつつある，「ジェネレーターを作ったり、遅延評価してみる」シリーズを Perl 6 で書いてみることにした．ちなみに筆者の Perl 歴は1日である．

---

## これまでの流れ

- [Ruby の Enumerator でジェネレータを作ったり、遅延評価してみる - ryota-ka's blog](http://ryota-ka.hatenablog.com/entry/2015/04/23/010520)
- [Python でジェネレータを作ったり、遅延評価してみる – ymyzk’s blog](https://blog.ymyzk.com/2015/04/python-generator-lazy/)
- [ECMAScript 6 でジェネレータを作ったり、遅延評価してみる – ymyzk’s blog](https://blog.ymyzk.com/2015/04/ecmascript-6-generator-lazy/)
- [Rust でジェネレータを作ったり、遅延評価してみる - ryota-ka's blog](http://ryota-ka.hatenablog.com/entry/2015/05/18/145113)
- [Swift でジェネレータを作ったり、遅延評価してみる – ymyzk’s blog](https://blog.ymyzk.com/2015/05/swift-generator-lazy/)
- [PHP でジェネレータを作ったり遅延評価してみる - たにしきんぐダム](http://tanishiking24.hatenablog.com/entry/2015/07/30/164111)
- [Scala でジェネレータを作ったり、遅延評価してみる - たにしきんぐダム](http://tanishiking24.hatenablog.com/entry/scala-generator)

## 実行環境について

OS X をお使いの場合，Homebrew から Rakudo Star がインストールできる．

```sh
$ brew install rakudo-star
```

それ以外の環境の方は自分でなんとかしてほしい．

シェルに `perl6` と打ち込めば REPL が起動する．以下に示す Perl 6 のコードの大半は，この REPL 上での入出力を貼り付けたものである．ただし，可読性の観点から，適宜改行などを補っている場合がある．複数行のコードを記述する際などはこの限りではないが，その場合はコードの先頭に shebang を書いてある．

## 匿名関数

Perl といえばやはり (？) サブルーチンだが，Perl 6 からは匿名関数が書けるようになった．

### $f(x) = 2x$

```perl
#!/usr/bin/env perl

my $double = sub { my $x = shift; return $x * 2; };
print $double->(10); # 20
```

```perl6
> my $double = -> $x { $x * 2 }
-> $x { #`(Block|140434789794168) ... }`}

> $double(10)
20
```

これは `$_` (topic variable) を用いて次のように書くこともできる．

```perl6
> my $dbl = { $_ * 2 }
-> ;; $_? is raw { #`(Block|140478925621680) ... }

> $dbl(10)
20
```

### $f(x, y) = x + y$

```perl
#!/usr/bin/env perl

my $add2 = sub {
  my ($a, $b) = @_;
  return $a + $b;
};
print $add2->(10, 20); # 30
```

```perl6
> my $add2 = -> $a, $b { $a + $b }
-> $a, $b { #`(Block|140302336391256) ... }

> $add2(10, 20)
30
```

また，これは以下のように書くこともできる．

```perl6
> my $add2_ = * + *
WhateverCode.new

> $add2_(10, 20)
30
```

## ジェネレータ (`Range` / `Seq`)

### 自然数

以下のように自然数全体の列を得ることができる．

```perl6
> 0..Inf
0..Inf

> 0..* # same as above
0..Inf
```

### 算術数列 / 幾何数列

算術数列と幾何数列は，最初に与える幾つかの項から自動的に導出することができる．

```perl6
> my @odd-numbers = 1, 3 ... *
[...]

> my @multiples-of-five = 0, 5 ... *
[...]

> my @powers-of-two = 1, 2, 4 ... *
[...]
```

[postcircumfix `[ ]`](http://doc.perl6.org/language/operators#postcircumfix_%5B_%5D) (array indexing operator) を用いることで，特定の要素を取り出したり，部分列を得たりすることができる．列が具体的にどのような値を持っているのかを確かめるのに便利である．

また，[prefix `^` ](http://doc.perl6.org/language/operators#prefix_%5E) (_upto_ operator) を用いて `^10` と書けば，`0..^10` という `Range` を得ることができる．これは終端である `10` を含まない，`0` から `9` までの整数の列である．

これらを用いて，先程の数列たちのそれぞれ最初の10項を表示してみよう．

```perl6
> @odd-numbers[^10] # (1, 3 ... *)[^10]
(1 3 5 7 9 11 13 15 17 19)

> @multiples-of-five[^10] # (0, 5 ... *)[^10]
(0 5 10 15 20 25 30 35 40 45)

> @powers-of-two[^10] # (1, 2, 4 ... *)[^10]
(1 2 4 8 16 32 64 128 256 512)
```

## `map` / `grep`

`map` や `grep` を用いて，既存の列から新たな列を生成することもできる．

```perl6
> my @squares = map { $_ ** 2 }, (0..Inf)
[...]

> @squares[^10]
(0 1 4 9 16 25 36 49 64 81)
```

```perl6
> my @cubes = (0..Inf).map: { $_ ** 3 }
[...]

> @cubes[^10]
(0 1 8 27 64 125 216 343 512 729)
```

```perl6
> (1..*).grep({ $_ %% 3 || $_ ~~ /3/ })
(3 6 9 12 13 15 18 21 23 24 27 30 31 32 33 34 35 36 37 38 39 42 43 45 48 51 53 54 57 60 63 66 69 72 73 75 78 81 83 84 87 90 93 96 99 102 103 105 108 111 113 114 117 120 123 126 129 130 131 132 133 134 135 136 137 138 139 141 143 144 147 150 153 156 159 162 163 165 168 171 173 174 177 180 183 186 189 192 193 195 198 201 203 204 207 210 213 216 219 222 ...)
```

cf.

- [infix `%%`](http://doc.perl6.org/language/operators#infix_%25%25) (divisibility operator)
- [infix `~~`](http://doc.perl6.org/language/operators#infix_~~) (smartmatch operator)

Perl のお家芸である pattern matching が `=~` じゃなくなってしまったのは少しさみしい気もする．

## 列の終了条件

`...` の後ろに関数を書くことで，列の終了条件を与えることができる．

```perl6
> 1, 2, 4 ... { $_ > 200 }
(1 2 4 8 16 32 64 128 256)

> 1, 2, 4 ... * > 200 # same as above
(1 2 4 8 16 32 64 128 256)
```

`...^` の場合には，最後の項は含まない．

```perl6
> 1, 2, 4 ...^ * > 200
(1 2 4 8 16 32 64 128)
```

## 漸化式

漸化式を用いて列を生成することもできる．

例として，

$$
a_{n+1} =
\begin{cases}
3a_n+1 & (\text{if } a_n \text{ is odd}) \\
\frac{1}{2}a_n & (\text{if } a_n \text{ is even})
\end{cases}
$$

が定める列を考える．ただし $a_k = 1$ となる $k$ が存在する場合，$a_k$ をもって打ち止めとする．

```perl6
> my $collatz = { $_, { $_ % 2  ??  $_ * 3 + 1  !!  $_ / 2 } ... * == 1 }
-> ;; $_? is raw { #`(Block|140474102374016) ... }

> $collatz(6)
(6 3 10 5 16 8 4 2 1)

> $collatz(27)
(27 82 41 124 62 31 94 47 142 71 214 107 322 161 484 242 121 364 182 91 274 137 412 206 103 310 155 466 233 700 350 175 526 263 790 395 1186 593 1780 890 445 1336 668 334 167 502 251 754 377 1132 566 283 850 425 1276 638 319 958 479 1438 719 2158 1079 3238 1619 4858 2429 7288 3644 1822 911 2734 1367 4102 2051 6154 3077 9232 4616 2308 1154 577 1732 866 433 1300 650 325 976 488 244 122 61 184 92 46 23 70 35 106 ...)
```

`{ $_ %% 2 ?? $_ / 2 !! $_ * 3 + 1 }` (ただし [infix `?? !!`](http://doc.perl6.org/language/operators#infix_%3F%3F_%21%21) は三項演算子) の部分が漸化式である．

与えられた関数の arity が n のとき，列の先行する n 項が引数として渡される．以下に arity が2の場合 (隣接3項間漸化式) の例を示す．

```perl6
> (0, 1, -> $a, $b { $a + $b } ... *)[^10]
(0 1 1 2 3 5 8 13 21 34)
```

前述したように `* + *` という形を使えば，次のように書くこともできる．

```perl6
> (0, 1, * + * ... *)[^10]
(0 1 1 2 3 5 8 13 21 34)
```

無事にフィボナッチ数列を得ることができた．以上の結果を組み合わせて，いつもと同じように，フィボナッチ数列の要素を

- それぞれ二乗し
- 奇数のもののみ選択し
- 先頭から10要素を取り出した

列を得てみよう．

```perl6
> (0, 1, * + * ... *).map({ $_ ** 2 }).grep({ $_ % 2 })[^10]
(1 1 9 25 169 441 3025 7921 54289 142129)
```

## `gather` / `take`

以上に示した方法は，種々の列を得るためにはかなり強力なものであり，多くのケースでは事足りると思うのだが，それでも物足りない場合には，もちろん手続き的にジェネレータを書くこともできる．次に示すのは，[`gather/take`](http://doc.perl6.org/language/control#gather%2Ftake) を使って実装した，フィボナッチ数列のジェネレータの例である．

```perl6
#!/usr/bin/env perl6

sub fibonacci() {
  gather {
    my ($a, $b) = 0, 1;
    loop {
      take $a;
      ($a, $b) = $b, $a + $b;
    }
  }
}

say fibonacci[0]; # 0
say fibonacci[1]; # 1
say fibonacci[2]; # 1
say fibonacci[3]; # 2

say fibonacci().map({ $_ ** 2 }).grep({ $_ % 2 })[^10];
# (1 1 9 25 169 441 3025 7921 54289 142129)
```

`gather` でコルーチンを生成し，`take` で値を yield することができる．

## まとめ

今回の記事のために初めて Perl を書いたのだが，やはり "P" から始まる言語はだいたい難しい．

26日以降も延長戦を続けてきた CAMPHOR- Advent Calendar 2015 も明日でついにおしまい．最後の記事は，CAMPHOR- 5期代表である @yaitaimo が今年の総括を書いてくださるそうだ．明日もお楽しみに！

## 参考リンク

- [class Range](http://doc.perl6.org/type/Range)
- [class Seq](http://doc.perl6.org/type/Seq)
- [Operators](http://doc.perl6.org/language/operators)
- [Control flow](http://doc.perl6.org/language/control)
- [Day 4 – The Sequence Operators | Raku Advent Calendar](https://perl6advent.wordpress.com/2010/12/04/the-sequence-operator/)
- [Day 23: Lazy fruits from the gather of Eden | Raku Advent Calendar](https://perl6advent.wordpress.com/2009/12/23/day-23-lazy-fruits-from-the-gather-of-eden/)
- [Perl6 のフィボナッチ数列生成についての解説 - tokuhirom's blog](http://blog.64p.org/entry/2015/10/27/103325)
