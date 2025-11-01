---
keywords:
  - Nix
---

# Nix flakeからprivateなリソースにアクセスする

[HERP](https://herp.co.jp/)では，開発体験の向上のため，順次[flakes](https://nix.dev/concepts/flakes.html)の導入を進めている．「次世代のNix」とでも呼ぶに相応しいflakesは，未だ実験的な機能と位置付けられているものの，従来のNixでは保証し切れなかったより高いビルドの再現性や，統一的なインタフェースの規定，刷新されたコマンド体系など，様々な改良が盛り込まれている．

再現性への追求に対する代償として，flakesは純粋性に関して従来よりも厳しい制約を設けている．特に`netrcImpureEnvVars`が利用できないため，privateなリソースにアクセスしたい場合に[前回の記事](https://blog.ryota-ka.me/posts/2022/05/01/handling-private-resources-with-nix)で紹介した方法が利用できない．そこで本稿では，flakeからprivateなリソースにアクセスする方法を紹介する．

---

## Flakeのinputとして利用する場合

ここでは例として，GitHub上の`your-company/private-repo`というprivate repositoryをflakeのinputとして利用するケースを想定する．

```nix filename=flake.nix
{
  inputs = {
    private-repo.url = "github:your-company/private-repo";
  };
}
```

GitHubやGitLabなど，アクセストークンを通じた認証機構を提供するプラットフォームからリソースを取得したい場合には，[`access-tokens`](https://nixos.org/manual/nix/stable/command-ref/conf-file.html#conf-access-tokens)オプションが利用できる．このオプションには，`host=token`という形式の文字列をスペースで区切った値を指定する．以下では`gho_XXXXXXXX`がGitHubのアクセストークンであるとする．

マシン上にグローバルに設定する場合は，`/etc/nix/nix.conf`に`access-tokens`オプションを指定する．

```config filename=/etc/nix/nix.conf
access-tokens = github.com=gho_XXXXXXXX
```

特定のユーザのみに設定する場合は，`$XDG_CONFIG_HOME/nix/nix.conf`に`extra-access-tokens`オプション[^1]を指定する．

```config filename=~/.config/nix/nix.conf
extra-access-tokens = github.com=gho_XXXXXXXX
```

個人の開発環境では，[personal access tokenを発行し](https://github.com/settings/personal-access-tokens/new)，上記のような設定を行うとよいだろう．一方，Nixに限った話ではないが，CI上で利用するアクセストークンの発行には一考を要する．一つの解決策としては，必要なリポジトリに対するread権限を持たせたGitHub appを用意しておくことで，ジョブごとに短命なアクセストークンを発行することができる．HERPでは，self-hosted runnerの`$ACTIONS_RUNNER_HOOK_JOB_STARTED`に指定したシェルスクリプト内でアクセストークンを発行し，前述の形式で`nix.conf`に書き込むようにしている．こうすることで，各リポジトリでは何ら特別な設定を行うことなく，`inputs`に指定したリソースを取得できる．

## その他一般のリソースの場合

ここでは例として，privateなnpm packageに依存したパケッジをビルドするために`yarn2nix`を利用するケースを想定する．また，npm packageはAWS CodeArtifact上にホストされているものとする．Flakesを利用するという以外の状況設定は前回の記事と同様であるため，そちらも合わせて参照されたい．

こうしたケースでは，`builtins.fetchurl`は`pkgs.fetchurl`とは異なり[`netrc-file`](https://nixos.org/manual/nix/stable/command-ref/conf-file.html#conf-netrc-file)オプションを参照してくれるという性質が利用できる．このオプションには`netrc`ファイルへの絶対パスを指定する．

AWS CodeArtifactのアクセストークンのような，短命な認可情報をオプションとして指定する場合には，前節で紹介した`nix.conf`に書き込む方式ではなく，`nix`コマンドのオプションを指定するのがよいだろう．ビルドに先立ち，前回の記事で紹介した内容が記述された`netrc`ファイルをカレントディレクトリ中に生成しておけば，以下のようなコマンドでビルドが行える．

```sh
$ nix --netrc-file "$PWD/netrc" build
```

ただし，`builtins.fetchurl`はSHA-512に対応していない[^2]という問題があるため，SHA-512のハッシュ値を要求するYarnなどと組み合わせて使う場合には実のところうまく機能しない．

幸い，[`NixOS/nix`リポジトリの`src/libexpr/fetchurl.nix`](https://github.com/NixOS/nix/blob/master/src/libexpr/fetchurl.nix)で定義されている関数が，SHA-512に対応する代替品として使用できる．`src/libexpr/`以下のファイルのパスは`<nix/PATH>`という形で参照できるため，この関数は`import <nix/fetchurl.nix>`というexpressionで得られる．

`yarn2nix`で生成したNixファイルが`./nix/yarn.nix`に存在している場合，以下のようなNix expressionを書けばよいだろう．

```nix filename=flake.nix
let
  dependencies = pkgs.callPackage ./nix/yarn.nix {
    fetchurl = import <nix/fetchurl.nix>; # fetchurlを上書きする
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

この記事の内容は，インターンである[@kgtkr](https://github.com/kgtkr)による調査の成果に負うところが大きい．感謝します．

## 広告

株式会社HERPでは，プロダクト開発に集中できる環境を整備できるエンジニアを募集している．

https://github.com/herp-inc/engineering-careers

また，Nix日本語コミュニティでは，Discord上でNixに関する情報交換を行っている．

https://github.com/nix-ja

## 脚注

[^1]: `extra-`を付けることで，グローバルの設定を上書きせずに追加で値を設定できる．
[^2]: 引数に`sha512`attributeを受け付けない．
