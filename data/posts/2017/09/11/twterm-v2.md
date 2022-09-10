---
keywords:
  - twterm
---

# エンジニアのための Twitter クライアント twterm v2 をリリースした

ターミナルエミュレータ上で動作する [twterm](https://twterm.ryota-ka.me/) の v2 をリリースした．

![screencast](https://twterm.ryota-ka.me/screencast.gif)

https://twterm.ryota-ka.me/

## これはなに

端末エミュレータ上で動作する Twitter クライアント．2015年1月ごろから細々と開発をしている．僕は普段 tmux でいくつも pane, window, session を作って作業をしているが，そのうちの一つで twterm を常に開いている．[Text-based User Interface](https://en.wikipedia.org/wiki/Text-based_user_interface) を採用していて，ターミナルから出ることなくインタラクティブに Twitter を眺めたり，ポストしたりできて便利．

---

## インストール

```sh
$ gem install twterm
```

ncurses と readline が有効化された Ruby ランタイムが必要．

## 変更点とか

### カスタマイズ可能なキー設定

`~/.twterm/keys.toml` を編集することで，キー設定をカスタマイズできるようになった．一部方面からは実装を切望 (？) されていた機能だったが，実際にどのくらい需要があるのかはわからない．デフォルトでは less(1) や vim(1) を意識したキー設定になっている．

### タブ内でのテキスト検索

一部のタブにおいて，more(1) や less(1) 風のテキスト検索をサポートした．テキスト検索デフォルトでは `/` キーで順方向への，`?` キーで逆方向への検索が行える．`n` / `N` キーを押せば次 / 前の検索結果に移動できる．

### リスト管理

ユーザをリストに追加したり，リストから削除したりできるようになった．Twitter のヘビーユーザはだいたいリスト機能に依存している気がするので，シュッと管理ができるといい感じ．

### 宣言的なレンダリング API

これは開発者[^1]が嬉しい機能なのだが，これまで極端に手続き的だった ncurses を用いた画面描画のインタフェースが劇的に改善され，宣言的な画面描画を行えるようになった[^2]．

これまで，描画部分は地獄みたいなコードベースだったので，この部分が改善されたことにより，開発の効率が随分と向上した．画面を描画する部分と，描画される仮想的な画面を構築する部分とが分離されたことにより，view のレイヤがテスタブルになったことも嬉しい．

## nihongo ga utenai

お使いの環境に **Readline が有効化された** Ruby があることを確認してください．

## 最後に

気に入ったら star してね．

https://github.com/ryota-ka/twterm/stargazers

## 脚注

[^1]: 私しかいない
[^2]: 設計にあたっては Haskell の ncurses ラッパライブラリである [vty](https://hackage.haskell.org/package/vty) のインタフェースを参考にした．
