---
keywords:
  - Lazy evaluation
  - Vim script
---

# Vim script でジェネレータを作ったり、遅延評価してみる

この記事は [CAMPHOR- Advent Calendar 2016](http://advent.camph.net/) 8日目の記事です．

## はじめに

日本時間の2016年9月12日に，Vim 8.0 がリリースされた．Vim 7.4 のリリースからはおよそ3年振り，Vim 7.0 からは実におよそ10年振りのヴァージョンアップだそうだ．Vim 8.0 では様々な新機能が追加されたが，中でも Vim script にラムダ式[^1]が追加されたのには目を引くものがあった．

ラムダ式の登場により，標準の `map()` 関数や `filter()` 関数の使い勝手が大幅に改善されたが，これらで遊んでいるうちに，似た操作をリストだけではなくイテレータに対して適用したいという欲求が自然と生じてきた．しかしながら，Vim script にはリストはあれど，イテレータなどというものは存在するはずもないので，今回自前で実装する運びとなった．

本稿では，まず初めに，ECMAScript 2015 のインタフェースに似た[^2]，すなわち，`next` を呼ぶと `{ value: 42, done: false }` という形式に近い値が返ってくるイテレータを Vim script で実装する．次に，このイテレータを返す関数，すなわちジェネレータを定義する．その後，イテレータを拡張し，`map` や `filter` などのよく知られたメソッドを定義することで，種々の操作を簡便に行えるようにする．

## これまでの流れ

- [Ruby の Enumerator でジェネレータを作ったり、遅延評価してみる - blog.ryota-ka.me](https://blog.ryota-ka.me/posts/2015/04/23/generators-and-lazy-evaluation-in-ruby)
- [Python でジェネレータを作ったり、遅延評価してみる – ymyzk’s blog](https://blog.ymyzk.com/2015/04/python-generator-lazy/)
- [ECMAScript 6 でジェネレータを作ったり、遅延評価してみる – ymyzk’s blog](https://blog.ymyzk.com/2015/04/ecmascript-6-generator-lazy/)
- [Rust でジェネレータを作ったり、遅延評価してみる - blog.ryota-ka.me](https://blog.ryota-ka.me/posts/2015/05/18/generators-and-lazy-evaluation-in-rust)
- [Swift でジェネレータを作ったり、遅延評価してみる – ymyzk’s blog](https://blog.ymyzk.com/2015/05/swift-generator-lazy/)
- [PHP でジェネレータを作ったり遅延評価してみる - たにしきんぐダム](http://tanishiking24.hatenablog.com/entry/2015/07/30/164111)
- [Scala でジェネレータを作ったり、遅延評価してみる - たにしきんぐダム](http://tanishiking24.hatenablog.com/entry/scala-generator)
- [Perl 6 でジェネレータを作ったり、遅延評価してみる - blog.ryota-ka.me](https://blog.ryota-ka.me/posts/2015/12/30/generators-and-lazy-evaluation-in-perl-6)

---

## 1 事前準備

イテレータ及びジェネレータを実装していくにあたり，事前に幾つか知識的な準備を行う．Vim script に詳しい読者は読み飛ばしてもらっても構わない．

### 1.1 ラムダ式

ラムダ式は Vim 8.0 の新機能[^3]で，`{ args -> expr }` という形式で簡潔に関数を定義することができる．

```vim
let s:Double = { x -> x * 2 }
echo s:Double(3)
" 6

let s:Add = { a, b -> a + b }
echo s:Add(4, 5)
" 9

let s:AlwaysFourtyTwo = { -> 42 }
echo s:AlwaysFourtyTwo()
" 42
```

また，ラムダ式は外側のスコープにある変数を参照できる．つまりクロージャとして振る舞う．以下では，2つの値を受け取り大きい方の値を返す，カリー化された `Max()` 関数を定義しているが，`Max()` のスコープにある `a:x` をラムダ式の内部から参照できている様子が見て取れる．

```vim
function! Max(x)
  return { y -> a:x >= y ? a:x : y }
endfunction

echo Max(100)(50)
" 100
```

また，ラムダ式を他の関数に引数として渡すこともできる．標準の `map()` 関数[^4]とともに用いる例を挙げる．

```vim
let s:xs = [0, 1, 2, 3, 4]
call map(s:xs, { x -> x * 2 })
echo s:xs
" [0, 2, 4, 6, 8]
```

### 1.2 Vim script におけるオブジェクト指向風プログラミング

ES2015-like なイテレータのインタフェースを実装したいため，まずは Vim script 上で，オブジェクト指向プログラミングを行えるように下準備を行う必要がある．オブジェクト指向とは何かという議論はここではしない．しかしながら，オブジェクトに関連付けられた値を持つことで，内部状態を表現し，また，オブジェクトに付随する関数の中でオブジェクト自身 (いわゆる `this` や `self`) の状態を読み出したり，あるいは変更したりできるならば，それは凡そオブジェクトと呼んで差し支えないであろうと思われる[^5]．また，似たような種類の異なるオブジェクトを複数生成するために，新たなオブジェクトを返す関数 (コンストラクタ) を用意しておくと便利である．

では，次の JavaScript のコードで示すような挙動を Vim script で実現してみよう．

```javascript
class Person {
  constructor(name) {
    this.name = name;
  }

  greet() {
    console.log(`Hey, I'm ${this.name}!`);
  }
}

const morishin = new Person('morishin');
const kohey = new Person('kohey');

morishin.greet();
// Hey, I'm morishin!

kohey.greet();
// Hey, I'm kohey!
```

対応する Vim script のコードを以下に示す．

```vim
function! Person(name)
  let person = { 'name': a:name }

  function! person.greet()
    echo "Hey, I'm " . self.name . "!"
  endfunction

  return person
endfunction

let s:morishin = Person("morishin")
let s:kohey = Person("kohey")

call s:morishin.greet()
" Hey, I'm morishin!

call s:kohey.greet()
" Hey, I'm kohey!
```

Vim script には辞書というデータ構造[^6]があり，オブジェクトとして振る舞わせるにはお誂え向きだ．ここで定義している `Person` という関数は，引数として `name` を受け取り，それを `'name'` というキーに関連付けて辞書に投げ込み，変数 `person` に代入している．この辞書こそが「オブジェクト」の実体である．

次に，この辞書に対して「メソッド」を定義している．まるで Ruby の特異メソッド定義のような書き方をしているが，このような記法を用いると，`greet` 関数を参照する `Funcref` を辞書の中に入れることができる．また，関数内では `self` を通じて辞書自身を参照できる[^7]．

このように定義した `Person` 関数を通じて，我々は「`Person` オブジェクト」とでも呼ぶべき辞書を手に入れることができるようになった．

## 2 イテレータ・ジェネレータの実装

事前の準備が済んだので，次にイテレータと，(イテレータ・オブジェクトを返す関数である) ジェネレータを定義する．

### 2.1 `Nat` ジェネレータ

まずは，もっとも単純だと思われる，`next` メソッドを呼び出す度に，後続の自然数を返し続けるイテレータを返すジェネレータ `Nat` を定義してみることにする．

参考までに，ECMAScript での実装を先に示しておく．

```javascript
function* Nat() {
  let i = 0;
  while (true) yield i++;
}

const nat = Nat();
nat.next(); // => { value: 0, done: false }
nat.next(); // => { value: 1, done: false }
nat.next(); // => { value: 2, done: false }
```

`next` メソッドを呼ぶと，`value` と `done` という2つのキーを持ったオブジェクトが返ってくるが，Vim script ではオブジェクトの代わりに辞書を用いる．対応する実装は以下である．

```vim
function! Nat()
  let it = {}
  let n = -1

  function! it.next() closure
    let n += 1
    return { 'value': n, 'done': v:false }
  endfunction

  return it
endfunction
```

空の辞書を作成し `it` に代入したのち，この辞書上に `next` メソッドを定義しているが，鍵になるのは，`function! it.next()` の後ろにある `closure` である．これは Vim 8.0 の新機能[^8]で，`closure` を伴って宣言された関数は，クロージャとして振る舞う，すなわち，関数の外側のスコープに存在する変数にアクセスできるようになる．これを利用して，`next` は評価される度に，その外側にある `n` の値をインクリメントした上で，`{ 'value': n, 'done': v:false }` という辞書を返すようにしてある．ここで，`done` に常に `v:false` を入れて返すのは，これまでにどれだけの値を yield したかに関わらず，常に次の値を yield できるということを示しており，任意の自然数について，その後者が存在することに対応している．そして `Nat` 自体は，こうして `next` メソッドを定義されたイテレータを返す．

こうして定義した `Nat` ジェネレータから得られるイテレータが，期待通り振る舞うことを確かめてみよう．

```vim
let s:nat = Nat()

echo s:nat.next()
" {'done': v:false, 'value': 0}

echo s:nat.next()
" {'done': v:false, 'value': 1}

echo s:nat.next()
" {'done': v:false, 'value': 2}

echo s:nat.next()
" {'done': v:false, 'value': 3}
```

### 2.2 `Range` ジェネレータ

Vim script には `range()` 関数が存在する．ヘルプ (`:help range`) の記述は以下の通りである．

```
range({expr} [, {max} [, {stride}]])				*range()*
		Returns a |List| with Numbers:
		- If only {expr} is specified: [0, 1, ..., {expr} - 1]
		- If {max} is specified: [{expr}, {expr} + 1, ..., {max}]
		- If {stride} is specified: [{expr}, {expr} + {stride}, ...,
		  {max}] (increasing {expr} with {stride} each time, not
		  producing a value past {max}).
		When the maximum is one before the start the result is an
		empty list.  When the maximum is more than one before the
		start this is an error.
		Examples: >
			range(4)		" [0, 1, 2, 3]
			range(2, 4)		" [2, 3, 4]
			range(2, 9, 3)		" [2, 5, 8]
			range(2, -2, -1)	" [2, 1, 0, -1, -2]
			range(0)		" []
			range(2, 0)		" error!
```

関数のインタフェースとしてはよくありそうなやつである．こいつは標準の関数なので，もちろんリストを返すのだが，イテレータを返すヴァージョンの `Range` を実装してみよう．

```vim
function! Range(expr, ...)
  let it = {}

  if a:0 == 0
    let [n, max, stride] = [0, a:expr - 1, 1]
  elseif a:0 == 1 || a:0 == 2
    let [n, max, stride] = [a:expr, a:1, a:0 == 1 ? 1 : a:2]
  else
    throw 'wrong number of arguments: Range({expr} [, {max} [, {stride}]])'
  endif

  function! it.next()
    let value = n
    let done = n > max ? v:true : v:false

    let n += stride

    return { 'value': done ? v:none : value, 'done': done }
  endfunction

  return it
endfunction
```

`Nat` の場合と違って，今回は有限列である．なので，`next` を呼ぶ度に終了条件を確認し，戻り値の辞書の `done` キーに含めている．また，返すべき値を持たない際には，`v:none` を返すようにしている[^9]．

```vim
let s:r1 = Range(3)

echo s:r1.next()
" {'done': v:false, 'value': 0}

echo s:r1.next()
" {'done': v:false, 'value': 1}

echo s:r1.next()
" {'done': v:false, 'value': 2}

echo s:r1.next()
" {'done': v:false, 'value': v:none}

echo s:r1.next()
" {'done': v:false, 'value': v:none}
```

```vim
let s:r2 = Range(3, 5)

echo s:r2.next()
" {'done': v:false, 'value': 3}

echo s:r2.next()
" {'done': v:false, 'value': 4}

echo s:r2.next()
" {'done': v:false, 'value': 5}

echo s:r2.next()
" {'done': v:true, 'value': v:none}

echo s:r2.next()
" {'done': v:true, 'value': v:none}
```

```vim
let s:r3 = Range(5, 10, 2)

echo s:r3.next()
" {'done': v:false, 'value': 5}

echo s:r3.next()
" {'done': v:false, 'value': 7}

echo s:r3.next()
" {'done': v:false, 'value': 9}

echo s:r3.next()
" {'done': v:true, 'value': v:none}

echo s:r3.next()
" {'done': v:true, 'value': v:none}
```

## 3 イテレータのメソッドを定義する

無事にジェネレータを定義できたので，次は趣向を変えて，イテレータ自体の利便性を高めていく．そのために，イテレータに `next` 以外のメソッドを実装していくことにしよう．

### 3.1 `Iterator` コンストラクタ

イテレータの出所が，`Nat` であれ `Range` であれ，あるいは他に定義したジェネレータであれ，すべてのイテレータが共通のメソッドを持っていてほしいというのは自然な欲求であろう．これを実現するために，拡張されたイテレータを生み出すための関数を先に定義しておく．上で見たようにイテレータの実体は，`next` というキーに `{'done': v:false, 'value': 42}` といった値を返す関数が入った単なる辞書であるから，イテレータ自身の振る舞いは，この関数ひとつで表現できるはずである．そこで，この関数を引数として取り，拡張されたイテレータを返す関数 `Iterator` を定義する．

```vim
function! Iterator(next)
  let it = { 'next': a:next }

  function! it.methodA(...)
    " do something
  endfunction

  function! it.methodB(...)
    " do something
  endfunction

  function it.methodC(...)
    " do something
  endfunction

  return it
endfunction
```

これに伴い，上で見た `Nat` および `Range` の定義も修正しておく．

```vim
function! Nat()
  let n = -1

  function! Next() closure
    let n += 1
    return { 'value': n, 'done': v:false }
  endfunction

  return Iterator(funcref('Next'))
endfunction
```

```vim
function! Range(expr, ...)
  if a:0 == 0
    let [n, max, stride] = [0, a:expr - 1, 1]
  elseif a:0 == 1 || a:0 == 2
    let [n, max, stride] = [a:expr, a:1, a:0 == 1 ? 1 : a:2]
  else
    throw 'wrong number of arguments: Range({expr} [, {max} [, {stride}]])'
  endif

  function! RangeNext() closure
    let value = n
    let done = n > max ? v:true : v:false

    let n += stride

    return { 'value': done ? v:none : value, 'done': done }
  endfunction

  return Iterator(funcref('RangeNext'))
endfunction
```

`Next` 関数[^10]を定義したのち，`funcref()` 関数で `Funcref` を得て，これを `Iterator` に渡している．`funcref()` は Vim 8.0 で新たに追加された[^11]関数で，`function()` と同じように `Funcref` を得るための関数だが，`function()` は名前による検索を行うために，同名の関数が再定義された際に新しいものを指してしまう一方，`funcref()` は参照による検索を行うため，そのような事態を避けることができる[^12]．

以上で準備が整ったので，実際にメソッドを定義していく．

### 3.2 `toList` メソッド

まず初めに実装するのは `toList` メソッドである．イテレータの `toList` メソッドを呼び出すと，イテレータの中に残っている値をすべて吐き出し，リストにして返す．ECMAScript の `Array.from()` をイテレータに適用するようなものだと考えてもらえればいいだろう．

以下にはメソッド定義部分だけを抜き出している．これは上で示した `Iterator` 関数の，`function! it.methodA(...)` などの部分に対応している．

```vim
function! it.toList()
  let xs = []

  while v:true
    let x = self.next()

    if x.done
      return xs
    endif

    call add(xs, x.value)

    unlet x
  endwhile
endfunction
```

`while v:true ~ endwhile` で無限ループを回しながら，リスト `xs` に要素を追加していき，`x.done` が真に評価された時点で `xs` を返す．

挙動を確かめてみよう．

```vim
echo Range(10).toList()
" [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

echo Range(25, 34).toList()
" [25, 26, 27, 28, 29, 30, 31, 32, 33, 34]

echo Nat().toList()
" won't stop
```

### 3.3 `take` メソッド

上で見たように，`Nat()` は無限列なので，リストに変換しようとすると評価が終了しない．しかし，列の要素を確認するために，列の先頭の要素を一部だけ得るといった操作ができると便利である．そこで，次に `take` メソッドを定義する．

```vim
function! it.take(n)
  let i = 0

  function! Next() closure
    if i < a:n
      let i += 1
      return self.next()
    endif

    return { 'value': v:none, 'done': v:true }
  endfunction

  return Iterator(funcref('Next'))
endfunction
```

最初の `n` 回の呼び出しだけ値を yield するような関数 `Next` を定義し，これを `Iterator` コンストラクタに渡して，新たに生成したイテレータを返している．

```vim
echo Nat().take(10).toList()
" [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
```

### 3.4 `map` メソッド

次に，既存の列の各要素に，与えられた関数を適用して，新たな列を返す `map` メソッドを定義する．

```vim
function! it.map(f)
  function! Next() closure
    let x = self.next()

    if x.done
      return { 'value': v:none, 'done': v:true }
    else
      return { 'value': a:f(x.value), 'done': v:false }
    endif
  endfunction

  return Iterator(funcref('Next'))
endfunction
```

`map` と `take` を用いれば，無限列である `Nat()` のすべての要素に関数を適用したのち，その一部分だけを取り出しているかのように振る舞わせることができる．

```vim
echo Nat().map({ x -> x * 2 }).take(10).toList()
" [0, 2, 4, 6, 8, 10, 12, 14, 16, 18]
```

### 3.5 `find` メソッド

後述の `filter` メソッドのために，先に `find` メソッドを定義しておく．これは，述語 (predicate) を受け取り，評価結果が真であるような最初の要素を返すメソッドである．

```vim
function! it.find(f)
  while v:true
    let x = self.next()

    if x.done
      return v:none
    elseif a:f(x.value)
      return x.value
    endif

    unlet x
  endwhile
endfunction
```

単にループの中で，与えられた述語を満たす要素を探して返しているだけであるが，該当する要素が見つからなかった場合には `v:none` を返す．

```vim
echo Nat().find({ x -> x * x > 100 })
" 11

echo Range(10).find({ x -> x == 42 })
" v:none
```

### 3.6 `filter` メソッド

最後に，述語を受け取り，評価結果が真であるような要素のみを yield するイテレータを返す `filter` メソッドを実装する．

```vim
function! it.filter(f)
  function! Next() closure
    let value = self.find(a:f)

    return { 'value': value, 'done': type(value) == type(v:none) }
  endfunction

  return Iterator(funcref('Next'))
endfunction
```

これは，先程定義した `find` メソッドを用いて簡単に定義できる．`find` から `v:none` が返ってきた場合，つまり，もはや条件に一致する要素が見当たらない場合には `done` に `v:true` が入るようになっている．

```vim
echo Nat().filter({ x -> x % 3 == 0 }).take(5).toList()
" [0, 3, 6, 9, 12]
```

### 3.7 `Fib` ジェネレータへの操作

さて，道具立てが整ったので，このシリーズでは毎度おなじみの，フィボナッチ数列の要素をそれぞれ二乗し，そのうち奇数のもののみを先頭から10要素取り出したリストを作ってみる．

まずは，フィボナッチ数列を得るためのジェネレータ `Fib` を定義する．

```vim
function! Fib()
  let [a, b] = [0, 1]

  function! Next() closure
    let value = a
    let [a, b] = [b, a + b]

    return { 'value': value, 'done': v:false }
  endfunction

  return Iterator(funcref('Next'))
endfunction
```

`Fib` が返すイテレータに対し，メソッド・チェーン・スタイルで行いたい操作を書けば，所望の結果が得られる．

```vim
echo Fib().map({ x -> x * x }).filter({ x -> x % 2 == 1 }).take(10).toList()
" [1, 1, 9, 25, 169, 441, 3025, 7921, 54289, 142129]
```

## 4 コード全文

今回使用したコードをまとまった形で書いておく．なお，上記で定義したメソッド以外にも，幾つかのメソッドが追加で定義されている．

```vim
function! Iterator(next)
  let it = { 'next': a:next }

  function! it.concat(other)
    function! Next() closure
      let x = self.next()
      if !x.done
        return { 'value': x.value, 'done': v:false }
      endif

      let y = a:other.next()
      if !y.done
        return { 'value': y.value, 'done': v:false }
      endif

      return { 'value': v:none, 'done': v:true }
    endfunction

    return Iterator(funcref('Next'))
  endfunction

  function! it.count()
    return len(self.toList())
  endfunction

  function! it.drop(n)
    for i in range(a:n)
      call self.next()
    endfor

    return self
  endfunction

  function! it.filter(f)
    function! Next() closure
      let value = self.find(a:f)

      return { 'value': value, 'done': type(value) == type(v:none) }
    endfunction

    return Iterator(funcref('Next'))
  endfunction

  function! it.find(f)
    while v:true
      let x = self.next()

      if x.done
        return v:none
      elseif a:f(x.value)
        return x.value
      endif

      unlet x
    endwhile
  endfunction

  function! it.flatMap(f)
    let iters = self.map(a:f)
    let iter = iters.next()

    function! Next() closure
      while v:true
        if iter.done
          return { 'value': v:none, 'done': v:true }
        endif

        let x = iter.value.next()

        if x.done
          let iter = iters.next()
          continue
        endif

        return { 'value': x.value, 'done': v:false }
      endfor
  endfunction

    return Iterator(funcref('Next'))
  endfunction

  function! it.head()
    return self.find({ -> v:true })
  endfunction

  function! it.map(f)
    function! Next() closure
      let x = self.next()

      if x.done
        return { 'value': v:none, 'done': v:true }
      else
        return { 'value': a:f(x.value), 'done': v:false }
      endif
    endfunction

    return Iterator(funcref('Next'))
  endfunction

  function! it.reduce(init, f)
    let xs = self.toList()
    let acc = a:init

    for x in xs
      let acc = a:f(acc, x)
    endfor

    return acc
  endfunction

  function! it.take(n)
    let i = 0

    function! Next() closure
      if i < a:n
        let i += 1
        return self.next()
      endif

      return { 'value': v:none, 'done': v:true }
    endfunction

    return Iterator(funcref('Next'))
  endfunction

  function! it.toList()
    let xs = []

    while v:true
      let x = self.next()

      if x.done
        return xs
      endif

      call add(xs, x.value)

      unlet x
    endwhile
  endfunction

  function! it.zip(other)
    return self.zipWith({ x, y -> [x, y] }, a:other)
  endfunction

  function! it.zipWith(f, other)
    function! Next() closure
      let x = self.next()
      let y = a:other.next()

      if x.done || y.done
        return { 'value': v:none, 'done': v:true }
      endif

      return { 'value': a:f(x.value, y.value), 'done': v:false }
    endfunction

    return Iterator(funcref('Next'))
  endfunction

  return it
endfunction

function! Nat()
  let n = -1

  function! Next() closure
    let n += 1
    return { 'value': n, 'done': v:false }
  endfunction

  return Iterator(funcref('Next'))
endfunction

function! Range(expr, ...)
  if a:0 == 0
    let [n, max, stride] = [0, a:expr - 1, 1]
  elseif a:0 == 1 || a:0 == 2
    let [n, max, stride] = [a:expr, a:1, a:0 == 1 ? 1 : a:2]
  else
    throw 'wrong number of arguments: Range({expr} [, {max} [, {stride}]])'
  endif

  function! RangeNext() closure
    let value = n
    let done = n > max ? v:true : v:false

    let n += stride

    return { 'value': done ? v:none : value, 'done': done }
  endfunction

  return Iterator(funcref('RangeNext'))
endfunction

function! Fib()
  let [a, b] = [0, 1]

  function! Next() closure
    let value = a
    let [a, b] = [b, a + b]

    return { 'value': value, 'done': v:false }
  endfunction

  return Iterator(funcref('Next'))
endfunction
```

## おわりに

本稿では，Vim 8 で実装された新しい言語機構を用いて，Vim script によるジェネレータ・イテレータの実装を行った．これまでの「ジェネレータを作ったり、遅延評価してみる」シリーズが，言語内の既存の実装をなぞるものだったのに対し，今回は，与えられた言語の中で，新たに機能の体系を築き上げた点において，有意義な内容になったのではないかと思う．

今回取り上げた内容自体については，実は Vim 8.0 リリース当初から構想があり，早いうちから実装の基本部分は終えていた．その頃には 8.0.0005 だったヴァージョン番号も，今や 8.0.0124 となっており，活発な開発状況を感じさせられる．なお，上記のコードの動作検証は Vim 8.0.0124 で行っている．

余談ではあるが，今回の実装にあたって，Vim 8.0 から導入された `test-functions` を使用した．これは，Vim プラグインなどの開発者向けに導入されたものだそうだが，今回のケースでも，`assert_equal()` 関数は非常に重宝した．

[CAMPHOR- Advent Calendar 2016](http://advent.camph.net/) 9日目の担当は [@shotarok](https://github.com/shotarok) です．お楽しみに！

## 脚注

[^1]: よくある話だが，ラムダ抽象相当のものが単に "lambda expression" と呼ばれている．Vim がオフィシャルにそう呼んでいるという事情を汲み，本稿もこれに従う．
[^2]: あくまで似ているだけで，互換ではない．特に，`next` メソッドが引数を受け取ることを想定しない．
[^3]: 正確には Vim 7.4.2044
[^4]: 破壊的で残念
[^5]: もちろん，異なるオブジェクトの特徴付けがあってもよいが．
[^6]: `:help Dictionary` を参照．
[^7]: 詳細は `:help numbered-function` ないし `:help anonymous-function` を参照のこと．
[^8]: 正確には Vim 7.4.2120
[^9]: 「`v:none` の用途として間違っているだろう」という意見は真っ当だと思うが，他にそれらしき値もないのでしようがない．
[^10]: `Range` の中で定義しているもののみ，手元にある assertion を実行した際に `Next` という名前だとエラーになってしまったので，`RangeNext` という名前にしてある．
[^11]: 正確には Vim 7.4.2137
[^12]: 詳しくは `:help funcref` を参照のこと．
