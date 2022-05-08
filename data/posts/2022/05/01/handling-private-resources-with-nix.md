---
keywords:
  - Nix
---

# Nix でのビルド時に private なリソースにアクセスする

[HERP](https://herp.co.jp/) では多くの成果物が [Nix](https://nixos.org/) を用いてビルドされている．例として，アプリケーションの Docker image，npm ライブラリの tarball，Helm chart を元にした Kubernetes の manifest ファイルなどが挙げられる．

"purely functional package manager" である Nix を利用すると高い再現性を持ったビルドを実現できるが，purely functional であるがゆえに，private なリソースに依存したビルドには一工夫が必要になる．private なリソースにアクセスするためには概して何らかの credential が必要になるからだ．そのような credential はビルド手順[^1]に残したくないし，仮に残したとしても，一定時間で expire したりするのでいずれにしても再現性が得られない．

もちろん，ビルド時に private なリソースを利用できないのでは営利企業内で利用するのは難しい．そこで，部分的に再現性を諦めるための抜け道が用意されている．本稿では `fetchurl` 関数の `netrcPhase` と `netrcImpureEnvVars` オプションを利用する方法を紹介する．

なお，本稿で単に `fetchurl` と書いた場合には，`builtins.fetchurl` ではなく [`NixOS/nixpkgs` リポジトリに定義されている `fetchurl` 関数](https://github.com/NixOS/nixpkgs/blob/153690d104f2d82264f8e10f37fcd8b812da0640/pkgs/build-support/fetchurl/default.nix#L40-L164)を指すものとする．

---

## 問題設定

最近，アプリケーションが依存する社内用パッケージのレジストリとして [AWS CodeArtifact](https://aws.amazon.com/codeartifact/) を利用し始めたので，これを具体例に挙げて解説する．

`https://mydomain-123456789.d.codeartifact.ap-northeast-1.amazonaws.com:443/npm/myrepository/private-package/-/private-package-0.1.0.tgz` という URL から `private-package` というパッケージを取得することを考える．また，パケッジマネジャーには Yarn を利用する．

## `netrcPhase` と `netrcImpureEnvVars`

`fetchurl` は HTTP を通じてリソースを取得するための関数である．`fetchurl` を利用して，認可を要する private なリソースにアクセスしたい場合，netrc ファイルが利用できる[^2]．

`fetchurl` は内部的に `curl` コマンドを利用してリソースを取得する[^3]．また，`fetchurl` には，`curl` が利用する netrc ファイルを生成する手順を伝えるための `netrcPhase` というオプション[^4]が用意されている．このオプションを指定すると，`curl` コマンドに `--netrc-file $PWD/netrc` というオプションを追加で渡してくれる[^5]．

また，重要な点として，ビルド時の環境に設定されている環境変数のうち，`netrcImpureEnvVars` オプション[^6]に指定した環境変数は `netrcPhase` の中で利用することができる．

これらを勘案すると，最終的に以下のような derivation を書けば目的が達成できることになる．

```nix
pkgs.fetchurl {
  name = "private-package";
  url = "https://mydomain-123456789.d.codeartifact.ap-northeast-1.amazonaws.com:443/npm/myrepository/private-package/-/private-package-0.1.0.tgz";

  # ここにホワイトリスト形式で指定した環境変数は netrcPhase 内で参照できる
  netrcImpureEnvVars = [
    "CREDENTIAL"
  ];

  # ./netrc ファイルを作成する
  # ここではビルド時の環境で設定されている環境変数 $CREDENTIAL が参照できる
  netrcPhase = ''
    do_something_with ''$CREDENTIAL
  '';
}
```

## netrc ファイルの内容

`fetchurl` に `netrcPhase` オプションを指定し，netrc ファイルを生成する手順を与えれば，生成された netrc ファイルを内部で使われている `curl` が利用するように取り計らってくれるということがわかった．次に問題になるのは，`netrcPhase` でどのような netrc ファイルを生成すればよいのかという点である．そこで Nix のことは一旦忘れ，どのような netrc ファイルを記述すれば，以下のような `curl` コマンドで CodeArtifact 上の package を取得できるかを明らかにしたい．

```sh
$ curl -LO --netrc-file ./netrc https://mydomain-123456789.d.codeartifact.ap-northeast-1.amazonaws.com:443/npm/myrepository/private-package/-/private-package-0.1.0.tgz
```

これには，天下り的だが，以下のような内容を記述すればよい．

```netrc filename=netrc
machine mydomain-123456789.d.codeartifact.ap-northeast-1.amazonaws.com
  password <authorization token>
```

ここで `<authorization token>` は `$ aws codeartifact get-authorization-token` で取得できる認可トークンである[^7]．

## tarball を取得する derivation

`curl` 経由で tarball を取得できたので，次は `fetchurl` を利用して同じ tarball を取得する derivation を作ってみよう．これまでの内容を組み合わせると，以下のような derivation を書けばよいことになる．

```nix filename=private-package.nix
{ pkgs ? import <nixpkgs> {} }:

pkgs.fetchurl {
  name = "private-package-0.1.0";
  url = "https://mydomain-123456789.d.codeartifact.ap-northeast-1.amazonaws.com:443/npm/myrepository/private-package/-/private-package-0.1.0.tgz";

  netrcImpureEnvVars = [
    "AWS_CODEARTIFACT_AUTHORIZATION_TOKEN"
  ];

  netrcPhase = ''
    cat > ./netrc << EOF
    machine mydomain-123456789.d.codeartifact.ap-northeast-1.amazonaws.com
      password ''$AWS_CODEARTIFACT_AUTHORIZATION_TOKEN
    EOF
  '';
}
```

必要な環境変数を設定した上でこの derivation をビルドすると，ビルド中に CodeArtifact から tarball をダウンロードすることができる．

```sh
$ export AWS_CODEARTIFACT_AUTHORIZATION_TOKEN=$(aws codeartifact get-authorization-token --domain mydomain --domain-owner 123456789 --query authorizationToken --output text)
$ nix-build ./private-package.nix
```

## Yarn との統合

実際の開発においては `private-package` 以外にも多数のライブラリに依存するので，上記のような derivation をいちいち手で書くわけにはいかない．`yarn2nix` を利用すれば，`yarn.lock` から自動的に derivation を生成してくれる．

```sh
$ nix-shell -p yarn2nix --run 'yarn2nix > ./nix/yarn.nix'
```

上記のコマンドを実行すると，以下のようなファイルが生成されているはずである．

```nix filename=nix/yarn.nix
{ fetchurl, fetchgit, linkFarm, runCommand, gnutar }: rec {
  offline_cache = linkFarm "offline" packages;
  packages = [
    # ...
    {
      name = "https___mydomain_123456789.d.codeartifact.ap_northeast_1.amazonaws.com_443_npm_myrepository__private_package_0.1.0.tgz";
      path = fetchurl {
        name = "https___mydomain_123456789.d.codeartifact.ap_northeast_1.amazonaws.com_443_myrepository_npm__private_package_0.1.0.tgz";
        url  = "https://mydomain-123456789.d.codeartifact.ap-northeast-1.amazonaws.com:443/npm/myrepository/private-package/-/private-package-0.1.0.tgz";
        sha1 = "0123456789abcdef0123456789abcdef01234567";
      };
    }
    # ...
  ];
}
```

ここで，引数の `fetchurl` には `netrcImpureEnvVars` および `netrcPhase` が指定された状態のものが渡される必要があるので，`fetchurl` を override しつつ `callPackage` する．

```nix filename=nix/offline-cache.nix
{ pkgs ? import <nixpkgs> {} }:

let
  dependencies = pkgs.callPackage (import ./yarn.nix) {
    fetchurl = opts: pkgs.fetchurl (opts // {
      netrcImpureEnvVars = [
        "AWS_CODEARTIFACT_AUTHORIZATION_TOKEN"
      ];
      netrcPhase = ''
        cat > ./netrc << EOF
        machine mydomain-123456789.d.codeartifact.ap-northeast-1.amazonaws.com
          password ''$AWS_CODEARTIFACT_AUTHORIZATION_TOKEN
        EOF
      '';
    });
  };
in

dependencies.offline_cache
```

この derivation は，以下のような記述をすることで，Yarn のオフラインキャッシュとして利用できる．

```nix filename=default.nix
{ pkgs ? import <nixpkgs> {} }:

let
  offline-cache = import ./nix/offline-cache.nix { inherit pkgs; };
in

pkgs.stdenv.mkDerivation {
  name = "web-app-with-private-dependency";
  version = "0.1.0";
  src = ./.;

  nativeBuildInputs = [
    pkgs.fixup_yarn_lock
    pkgs.yarn
  ];

  configurePhase = ''
    runHook preConfigure

    export HOME=$TMP
    fixup_yarn_lock ./yarn.lock
    yarn config --offline set yarn-offline-mirror ${offline-cache}
    yarn install --frozen-lockfile --ignore-platform --ignore-scripts --no-progress --offline

    runHook postConfigure
  '';

  buildPhase = ''
    runHook preBuild

    yarn webpack

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mv ./dist $out

    runHook postInstall
  '';
}
```

## 謝辞

この記事の内容は，同僚である [@hiroqn](https://github.com/hiroqn) および [@ruicc](https://github.com/ruicc) による調査の成果に負うところが大きい．感謝します．

## 広告

株式会社 HERP では，プロダクト開発に集中できる環境を整備できるエンジニアを募集しています．

https://github.com/herp-inc/engineering-careers

## 脚注

[^1]: Nix であれば derivation
[^2]: https://nixos.wiki/wiki/Enterprise
[^3]: [実装](https://github.com/NixOS/nixpkgs/blob/153690d104f2d82264f8e10f37fcd8b812da0640/pkgs/build-support/fetchurl/builder.sh#L12)
[^4]: [実装](https://github.com/NixOS/nixpkgs/blob/153690d104f2d82264f8e10f37fcd8b812da0640/pkgs/build-support/fetchurl/default.nix#L67-L68)
[^5]: [実装](https://github.com/NixOS/nixpkgs/blob/153690d104f2d82264f8e10f37fcd8b812da0640/pkgs/build-support/fetchurl/default.nix#L159)
[^6]: [実装](https://github.com/NixOS/nixpkgs/blob/153690d104f2d82264f8e10f37fcd8b812da0640/pkgs/build-support/fetchurl/default.nix#L70-L72)
[^7]: `codeartifact:GetAuthorizationToken` だけでなく `sts:GetServiceBearerToken` の実行権限も必要なことに注意されたい．
