---
keywords:
  - Nix
---

# Nix flake から private なリソースにアクセスする

[HERP](https://herp.co.jp/) では，開発体験の向上のため，順次 [flakes](https://nix.dev/concepts/flakes.html) の導入を進めている．「次世代の Nix」とでも呼ぶに相応しい flakes は，未だ実験的な機能と位置付けられているものの，従来の Nix では保証し切れなかったより高いビルドの再現性や，統一的なインタフェースの規定，刷新されたコマンド体系など，様々な改良が盛り込まれている．

再現性への追求に対する代償として，flakes は純粋性に関して従来よりも厳しい制約を設けている．特に `netrcImpureEnvVars` が利用できないため，private なリソースにアクセスしたい場合に[前回の記事](https://blog.ryota-ka.me/posts/2022/05/01/handling-private-resources-with-nix)で紹介した方法が利用できない．そこで本稿では，flake から private なリソースにアクセスする方法を紹介する．

---

## Flake の input として利用する場合

ここでは例として，GitHub 上の `your-company/private-repo` という private repository を flake の input として利用するケースを想定する．

```nix filename=flake.nix
{
  inputs = {
    private-repo.url = "github:your-company/private-repo";
  };
}
```

GitHub や GitLab など，アクセストークンを通じた認証機構を提供するプラットフォームからリソースを取得したい場合には，[`access-tokens`](https://nixos.org/manual/nix/stable/command-ref/conf-file.html#conf-access-tokens) オプションが利用できる．このオプションには，`host=token` という形式の文字列をスペースで区切った値を指定する．以下では `gho_XXXXXXXX` が GitHub のアクセストークンであるとする．

マシン上にグローバルに設定する場合は，`/etc/nix/nix.conf` に `access-tokens` オプションを指定する．

```config filename=/etc/nix/nix.conf
access-tokens = github.com=gho_XXXXXXXX
```

特定のユーザのみに設定する場合は，`$XDG_CONFIG_HOME/nix/nix.conf` に `extra-access-tokens` オプション[^1]を指定する．

```config filename=~/.config/nix/nix.conf
extra-access-tokens = github.com=gho_XXXXXXXX
```

個人の開発環境では，[personal access token を発行し](https://github.com/settings/personal-access-tokens/new)，上記のような設定を行うとよいだろう．一方，Nix に限った話ではないが，CI 上で利用するアクセストークンの発行には一考を要する．一つの解決策としては，必要なリポジトリに対する read 権限を持たせた GitHub app を用意しておくことで，ジョブごとに短命なアクセストークンを発行することができる．HERP では，self-hosted runner の `$ACTIONS_RUNNER_HOOK_JOB_STARTED` に指定したシェルスクリプト内でアクセストークンを発行し，前述の形式で `nix.conf` に書き込むようにしている．こうすることで，各リポジトリでは何ら特別な設定を行うことなく，`inputs` に指定したリソースを取得できる．

## その他一般のリソースの場合

ここでは例として，private な npm package に依存したパケッジをビルドするために `yarn2nix` を利用するケースを想定する．また，npm package は AWS CodeArtifact 上にホストされているものとする．Flakes を利用するという以外の状況設定は前回の記事と同様であるため，そちらも合わせて参照されたい．

こうしたケースでは，`builtins.fetchurl` は `pkgs.fetchurl` とは異なり [`netrc-file`](https://nixos.org/manual/nix/stable/command-ref/conf-file.html#conf-netrc-file) オプションを参照してくれるという性質が利用できる．このオプションには `netrc` ファイルへの絶対パスを指定する．

AWS CodeArtifact のアクセストークンのような，短命な認可情報をオプションとして指定する場合には，前節で紹介した `nix.conf` に書き込む方式ではなく，`nix` コマンドのオプションを指定するのがよいだろう．ビルドに先立ち，前回の記事で紹介した内容が記述された `netrc` ファイルをカレントディレクトリ中に生成しておけば，以下のようなコマンドでビルドが行える．

```sh
$ nix --netrc-file "$PWD/netrc" build
```

ただし，`builtins.fetchurl` は SHA-512 に対応していない[^2]という問題があるため，SHA-512 のハッシュ値を要求する Yarn などと組み合わせて使う場合には実のところうまく機能しない．

幸い，[`NixOS/nix` リポジトリの `src/libexpr/fetchurl.nix`](https://github.com/NixOS/nix/blob/master/src/libexpr/fetchurl.nix) で定義されている関数が，SHA-512 に対応する代替品として使用できる．`src/libexpr/` 以下のファイルのパスは `<nix/PATH>` という形で参照できるため，この関数は `import <nix/fetchurl.nix>` という expression で得られる．

`yarn2nix` で生成した Nix ファイルが `./nix/yarn.nix` に存在している場合，以下のような Nix expression を書けばよいだろう．

```nix filename=flake.nix
let
  dependencies = pkgs.callPackage ./nix/yarn.nix {
    fetchurl = import <nix/fetchurl.nix>; # fetchurl を上書きする
  };
in

# ...

{
  packages.default = pkgs.callPackage ./default.nix {
    inherit (dependencies) offline_cache;
  };
}
```

## 謝辞

この記事の内容は，インターンである [@kgtkr](https://github.com/kgtkr) による調査の成果に負うところが大きい．感謝します．

## 広告

株式会社 HERP では，プロダクト開発に集中できる環境を整備できるエンジニアを募集している．

https://github.com/herp-inc/engineering-careers

また，Nix 日本語コミュニティでは，Discord 上で Nix に関する情報交換を行っている．

https://github.com/nix-ja

## 脚注

[^1]: `extra-` を付けることで，グローバルの設定を上書きせずに追加で値を設定できる．
[^2]: 引数に `sha512` attribute を受け付けない．
