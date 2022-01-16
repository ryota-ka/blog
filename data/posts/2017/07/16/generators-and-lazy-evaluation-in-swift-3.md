# Swift 3 でジェネレータを作ったり、遅延評価してみる

Swift でのジェネレータの取扱いや遅延評価については，ymyzk 先生の[『Swift でジェネレータを作ったり、遅延評価してみる』](https://blog.ymyzk.com/2015/05/swift-generator-lazy/)において解説されているが，2015年5月の情報といささか古く，Swift のヴァージョンも1.2だった頃の記事なので，改めて書くことにした．

2017年7月16日現在，Xcode 9.0 beta 上では Swift 4.0 がサポートされているが，手元の環境は Xcode 8.3.3 であるため，Swift 3.1 をベースに解説する．Apple の先走りで，ドキュメントのリンク先が一部 Swift 4 に関するものになっている部分があるが，本記事の理解にあたって本質的な違いはないはずである．

---

## これまでの流れ

- [Ruby の Enumerator でジェネレータを作ったり、遅延評価してみる - ryota-ka's blog](https://ryota-ka.hatenablog.com/entry/2015/04/23/010520)
- [Python でジェネレータを作ったり、遅延評価してみる – ymyzk’s blog](https://blog.ymyzk.com/2015/04/python-generator-lazy/)
- [ECMAScript 6 でジェネレータを作ったり、遅延評価してみる – ymyzk’s blog](https://blog.ymyzk.com/2015/04/ecmascript-6-generator-lazy/)
- [Rust でジェネレータを作ったり、遅延評価してみる - ryota-ka's blog](https://ryota-ka.hatenablog.com/entry/2015/05/18/145113)
- [Swift でジェネレータを作ったり、遅延評価してみる – ymyzk’s blog](https://blog.ymyzk.com/2015/05/swift-generator-lazy/)
- [PHP でジェネレータを作ったり遅延評価してみる - たにしきんぐダム](https://tanishiking24.hatenablog.com/entry/2015/07/30/164111)
- [Scala でジェネレータを作ったり、遅延評価してみる - たにしきんぐダム](https://tanishiking24.hatenablog.com/entry/scala-generator)
- [Perl 6 でジェネレータを作ったり、遅延評価してみる - ryota-ka's blog](https://ryota-ka.hatenablog.com/entry/2015/12/30/000000)
- [Vim script でジェネレータを作ったり、遅延評価してみる - ryota-ka's blog](https://ryota-ka.hatenablog.com/entry/2016/12/08/000000)

## `IteratorProtocol`

[`IteratorProtocol`](https://developer.apple.com/documentation/swift/iteratorprotocol) は，

> A type that supplies the values of a sequence one at a time.

と説明されるプロトコルで，かつては `GeneratorType` と呼ばれていたものである．`stdlib/public/core/Sequence.swift` で以下のように[^1]定義されている．

```swift
public protocol IteratorProtocol {
    associatedtype Element

    public mutating func next() -> Self.Element?
}
```

[_associated type_](https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Generics.html#//apple_ref/doc/uid/TP40014097-CH26-ID189) として `Element` という型をひとつ持つ _protocol_ で，イテレータが次に yield する値を `Element?` 型[^2]で返す `next()` メソッドを持っている．また，`next()` メソッドには `mutating` キーワードが付いているので，この際にデータ型の内部状態として持つ値は破壊的に変更してもよい[^3]．

試しに，`next()` を呼び出す度，自然数を順に返す `Nat` を定義してみよう．

```swift
struct Nat: IteratorProtocol {
    var n = 0

    mutating func next() -> Int? {
        defer { n += 1 }

        return n
    }
}

var nat = Nat()

print(nat.next()) // Optional(0)
print(nat.next()) // Optional(1)
print(nat.next()) // Optional(2)
print(nat.next()) // Optional(3)
print(nat.next()) // Optional(4)
```

ここで [`defer statement`](https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Statements.html#//apple_ref/doc/uid/TP40014097-CH33-ID532) というものが登場しているが，これは，`defer` が宣言されたスコープを抜ける際に，与えられた [_code block_](https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Declarations.html#//apple_ref/swift/grammar/code-block) を実行してくれるものである．言語機構として `yield` を持つ言語ならば，yield した後に値をインクリメントするといった処理が行えるが，構造体とメソッドしか持たない場合はそうもいかない．そこで，このような解決策を取っている．余談ではあるが，[Swift 3 からは昔ながらのC言語スタイルの `++` (および `--`) 演算子が削除された](https://github.com/apple/swift-evolution/blob/master/proposals/0004-remove-pre-post-inc-decrement.md)．

## `Sequence`

[`Sequence`](https://developer.apple.com/documentation/swift/sequence) は，かつては `SequenceType` と呼ばれていたもので，

> A type that provides sequential, iterated access to its elements.

と紹介されており，データ型をこのプロトコルに適合させると，`for-in` によるループ操作ができるようになる．データ型を `Sequence` プロトコルに適合させるためには， イテレータを返す `makeIterator()` メソッドを実装すればよいが，既に `ProtocolIterator` に適合している場合はもっと簡単で，単に `Sequence` に適合していることを宣言すればよい．これは以下のような宣言で実現されている[^4]．

```swift
/// A default makeIterator() function for `IteratorProtocol` instances that
/// are declared to conform to `Sequence`
extension Sequence where Self.Iterator == Self {
  /// Returns an iterator over the elements of this sequence.
  @_inlineable
  public func makeIterator() -> Self {
    return self
  }
}
```

`Sequence` は _associated type_ として，`IteratorProtocol` に適合するような `Iterator` を持っているのだが，この `Iterator` が自分自身のときには， `makeIterator()` が `self` を返すようなデフォルト実装が与えられている．

自然数の列は無限に長い[^5]ので，`for-in` ループでイテレーションを行うと停止せず，困ってしまう．そこで，与えられた自然数 n について，0 から n までの有限列を返す `Upto` イテレータを定義した上で，`for-in` の挙動を確かめてみよう．

```swift
struct Upto: IteratorProtocol, Sequence {
    let n: Int
    var curr = 0

    init(_ n: Int) {
        self.n = n
    }

    mutating func next() -> Int? {
        defer { curr += 1 }

        return curr <= n ? curr : nil
    }
}

for k in Upto(4) {
    print(k) // 0, 1, 2, 3, 4 が順に表示される
}
```

あまりにも馴染みがある記法なので特に目新しさはないが，自分で定義したデータ型が `for-in` ループで使用できていることがわかる．

## 遅延評価

`Sequence` は `lazy` という _instance property_ を持っているが，これは以下のようなシグネチャで表現されている．

```swift
extension Sequence {

    /// A sequence containing the same elements as this sequence,
    /// but on which some operations, such as `map` and `filter`, are
    /// implemented lazily.
    ///
    /// - SeeAlso: `LazySequenceProtocol`, `LazySequence`
    public var lazy: LazySequence<Self> { get }
}
```

これは `Self` を _generic parameter_ として持っている `LazySequence` を返す．これが `Sequence` の遅延版であり，`map` や `filter` などのメソッドが遅延評価されるように実装されている．これを用いて，いつも通りではあるが，フィボナッチ数列のそれぞれの要素を自乗し，奇数のもののみ抜き出したのち，先頭から10個の要素を取り出してみよう．

```swift
struct Fib: IteratorProtocol, Sequence {
    var (a, b) = (0, 1)

    mutating func next() -> Int? {
        defer { (a, b) = (b, a + b) }

        return a
    }
}

let xs: AnySequence<Int> = Fib().lazy
    .map({ $0 * $0 })
    .filter({ $0 % 2 != 0 })
    .prefix(10)

print(Array(xs)) // [1, 1, 9, 25, 169, 441, 3025, 7921, 54289, 142129]
```

ここで [`AnySequence`](https://developer.apple.com/documentation/swift/anysequence) という型が登場しているが，これは `Sequence` に 型消去 (type erasure) を施したもので，残念なことにこれがないと型推論がおかしくなって死ぬ．というのも，コンパイルには成功するが，メソッド呼び出しが何故か正格な方に解決されてしまい，大きな数を扱おうとして illegal hardware instruction で死ぬ．なんとも情けない話である．

## 参考資料

- [IteratorProtocol - Swift Standard Library | Apple Developer Documentation](https://developer.apple.com/documentation/swift/iteratorprotocol)
- [Sequence - Swift Standard Library | Apple Developer Documentation](https://developer.apple.com/documentation/swift/sequence)
- [AnySequence - Swift Standard Library | Apple Developer Documentation](https://developer.apple.com/documentation/swift/anysequence)
- [The Swift Programming Language (Swift 4): Statements](https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Statements.html)
- [The Swift Programming Language (Swift 4): Generics](https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Generics.html)
- [GitHub - apple/swift-evolution: This maintains proposals for changes and user-visible enhancements to the Swift Programming Language.](https://github.com/apple/swift-evolution)

## 脚注

[^1]: ただしコメント部は除く．
[^2]: 返すことができる値がない場合には `nil` を返す
[^3]: かつては associated type declaration のために `typealias` キーワードが用いられていたが，[Swift 2.2 からは `associatedtype` キーワードに変更されている．](https://github.com/apple/swift-evolution/blob/master/proposals/0011-replace-typealias-associated.md)
[^4]: https://github.com/apple/swift/blob/5256d774eaf774ec1e3c566aa8047764a1124837/stdlib/public/core/Sequence.swift#L625-L633
[^5]: もちろんメモリを考慮すればこの限りではない．
