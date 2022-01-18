# Dhall v3.0.0 がリリースされた

Dhall v3.0.0 がリリースされた．メイジャーバージョンの更新ということで，いくつかの破壊的変更を含む．

https://twitter.com/dhall_lang/status/1052242769916428288

---

## `Some` / `None` コンストラクタが予約語として導入

`Optional` 型のコンストラクタである `Some` / `None` が予約語として追加された．

https://github.com/dhall-lang/dhall-lang/pull/227

Dhall における `Optional` 型のリテラルは，`List` 型と同じく角括弧なので，リテラルで書く場合には `List` 型と区別するために型注釈が必須となっている．

```dhall
⊢ :t [42]
List Natural

⊢ :t [42] : Optional Natural
Optional Natural
```

毎回型注釈を書くのは面倒なので，これまでは [Prelude](https://github.com/dhall-lang/prelude) で適宜されている [`Optional/Some`](https://github.com/dhall-lang/Prelude/blob/302881a17491f3c72238975a6c3e7aab603b9a96/Optional/Some) と [`Optional/None`](https://github.com/dhall-lang/Prelude/blob/302881a17491f3c72238975a6c3e7aab603b9a96/Optional/None) をインポートして使うのが常套手段だった．

```dhall
⊢ :let Some = https://raw.githubusercontent.com/dhall-lang/Prelude/302881a17491f3c72238975a6c3e7aab603b9a96/Optional/Some
Some : ∀(a : Type) → ∀(v : a) → Optional a

⊢ Some Natural 42
[ 42 ] : Optional Natural
```

しかし，Dhall v3.0.0 からは `Some` と `None` の両コンストラクタが予約語として導入されることとなった．加えて `Some` は型推論をしてくれるので，`Type` 型の値を第1引数として渡さなくて済む．

```dhall
⊢ Some 42
[ 42 ] : Optional Natural
```

## 種多相のサポート

種多相 (kind-polymorphism) がサポートされた．型の型であるところの種 (`Kind`) の型を表すキーワードとして `Sort` が導入された．`Type : Kind : Sort` という関係である．

https://github.com/dhall-lang/dhall-lang/pull/238

モチベーションとしては，レコードの中に `Type` 以外の種を持つ型を入れられるようにしたかったようである．

https://github.com/dhall-lang/dhall-lang/pull/241

上記の pull request の変更によって，以下のような記述が許されることが期待される．

```dhall
⊢ { foo: Optional }
```

これまでは `Invalid field type` として弾かれていたが，diff を見ると，値として `Type` 型でない場合 (例えば `Type → Type` 型など) でもレコードの値として格納できるようになったことがわかる．

```diff
diff --git a/standard/semantics.md b/standard/semantics.md
index 1183e68..5c3ec0f 100644
--- a/standard/semantics.md
+++ b/standard/semantics.md
@@ -3501,13 +3501,13 @@ A record can either store term-level values and functions:
 ... or store types (if it is non-empty):


-    Γ ⊢ T :⇥ Kind   T ≡ Type
-    ────────────────────────
+    Γ ⊢ T :⇥ Kind
+    ────────────────────
     Γ ⊢ { x : T } : Kind


-    Γ ⊢ T :⇥ Kind   T ≡ Type   Γ ⊢ { xs… } :⇥ Kind
-    ──────────────────────────────────────────────  ; x ∉ { xs… }
+    Γ ⊢ T :⇥ Kind   Γ ⊢ { xs… } :⇥ Kind
+    ───────────────────────────────────  ; x ∉ { xs… }
     Γ ⊢ { x : T, xs… } : Kind
```

## その他

他にも，`∧` 演算子を使ってレコードを結合した際に，レコードのキーの順序が並び替えられることを保証するためのβ簡約規則が導入されたり，`Prelude` に `Integer/toDouble` と `Natural/toDouble` が追加されたりしている．詳細な changelog は GitHub 上で確認できる．

[](https://github.com/dhall-lang/dhall-lang/releases/tag/v3.0.0)
