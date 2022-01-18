# Ruby の Enumerator でジェネレータを作ったり，遅延評価してみる

Ruby には [Enumerable](http://docs.ruby-lang.org/ja/2.2.0/class/Enumerable.html) モジュールってのがあって，これを include したオブジェクトは，自身に対して何かしらの反復処理ができるようになる[^1]．
また，その反復処理を用いた `Enumerable#map` とか `Enumerable#select`[^2] とか `Enumerable#reduce` とかが使えるようになる．

更に，[`Enumerator`](http://docs.ruby-lang.org/ja/2.2.0/class/Enumerator.html) というものがある．`Enumerable` がモジュールなのに対して，`Enumerator` はクラスなので，インスタンス化できる．また，`Enumerator` は `Enumerable` を include している．この `Enumerator` は，外部イテレータ，いわゆるジェネレータとして使える．

---

フィボナッチ数列のジェネレータを作ってみよう．

```ruby
fib = Enumerator.new do |y|
  a, b = 0, 1
  loop do
    y << a
    a, b = b, a + b
  end
end

fib.next # => 0
fib.next # => 1
fib.next # => 1
fib.next # => 2
fib.next # => 3

fib.take(10) # => [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
```

`Enumerator.new` にブロックを渡すと，ブロック引数として `Enumerator::Yielder` のインスタンスが渡される．こいつの `<<` メソッドの引数として値を渡してやると，ジェネレータとして値を返して (yield して) くれる．

更に，[`Enumerator::Lazy`](http://docs.ruby-lang.org/ja/2.2.0/class/Enumerator=3a=3aLazy.html) というものもある．こちらは，`map`, `flat_map`, `select`, `reject`, `grep`, `take`, `drop`, `zip` などのメソッドが，即値で配列を返すのではなく，別の `Enumerator::Lazy` のインスタンスを返すようにオーバーライドされている．

先程のフィボナッチ数列のジェネレータの遅延評価バージョンを作ってみる．`Enumerable#lazy` を呼ぶと，`Enumerator::Lazy` のインスタンスが得られる．

```ruby
lazy_fib = fib.lazy

sq = -> x { x**2 }
lazy_fib.map(&sq).select(&:odd?).first(10)
# => [1, 1, 9, 25, 169, 441, 3025, 7921, 54289, 142129]
# first は eager

lazy_fib.map(&sq).select(&:odd?).take(10)
# => #<Enumerator::Lazy: ...>
# take は lazy
```

https://twitter.com/cotton_ori/status/590809351822053377

遅延評価！遅延評価！🎉

## 脚注

[^1]: `Enumerable` モジュールの中のメソッドは，すべて `each` メソッドを用いて処理されるので，`Enumerable` モジュールを include するモジュールには，`each` メソッドが実装されていることが求められる．正確に言うと，この `each` メソッドこそが，反復処理の挙動を規定する．
[^2]: 余談だが，`filter` じゃなくて `select` だというのをいつも忘れる．
