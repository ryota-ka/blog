---
keywords:
  - Haskell
  - Haskell IDE Engine
  - Nix
---

# NixでHaskell IDE Engineをシュッと入れる

みなさん，Haskellやってますか？普通に[Haskell IDE Engine](https://github.com/haskell/haskell-ide-engine)を入れようとするとビルドにメッチャ時間がかかって「地獄か？」という感じがするので，Nixでシュッと入れていきましょう．

---

## [Nix](https://nixos.org/nix/)のインストール

Nixを入れていない人はいないと思うけど，念の為インストール手順です．

https://twitter.com/hiroqn/status/1144580004275052544

```sh
$ curl https://nixos.org/nix/install | sh
```

```sh
$ nix-channel --add https://nixos.org/channels/nixpkgs-unstable
$ nix-channel --update
```

## [Cachix](https://cachix.org/)のインストール

Cachixだよ

Nixのバイナリキャッシュをホスティングしてくれるすごいやつだよ

```sh
$ nix-env -iA nixpkgs.cachix
```

## Haskell IDE Engineのインストール

まず`cachix`で`all-hies`のキャッシュを使うよう設定する．

```sh
$ cachix use all-hies
```

この状態で`nix-env -i`でインストールを試みると，cachixから諸々のバイナリキャッシュが降ってきて，比較的短時間でHaskell IDE Engineがインストールされるはず．

```sh
$ nix-env -iA selection --arg selector 'p: { inherit (p) ghc865; }' -f https://github.com/infinisil/all-hies/tarball/master
```

## デモ

[coc.nvim](https://github.com/neoclide/coc.nvim)を使って動かしてみる．

![screencast](https://gyazo.com/19da0d108e7fd19a833dc3fc419e928f.gif)

便利な世の中になりましたなぁ．
