# Nix でのビルド時に Puppeteer を使う場合には $FONTCONFIG_FILE を設定しよう

## TL; DR

```nix
FONTCONFIG_FILE = "${pkgs.fontconfig.out}/etc/fonts/fonts.conf";
```

---

## ビルドプロセスで Puppeteer を使いたい

前回の記事で，静的にビルドした Storybook に対し，Puppeteer を通じてヘッドレスモードの Chromium を立ち上げ，`stories.json` というファイルを生成する手順を紹介した．

https://blog.ryota-ka.me/posts/2021/01/31/storybook-composition

## Chromium を動かすのは面倒な場合がある

Puppeteer を用いれば Chromium の操作の自動化を手軽に行えるが，CI 上などで Chromium をインストールするためには，大量の動的ライブラリの存在が要求され，少々煩わしい．

以下は，かつて `.circleci/config.yml` に書いていた内容の抜粋である．

```sh
$ sudo apt-get install -y \
    libatk-bridge2.0-0 \
    libatk1.0-0 \
    libcups2 \
    libgtk-3-0 \
    libnss3 \
    libx11-xcb1 \
    libxcomposite1 \
    libxcursor1 \
    libxdamage1 \
    libxi6 \
    libxrandr2 \
    libxss1 \
    libxtst6
```

Docker イメージをビルドする際にも `metadata.json` を生成する必要があるため，`Dockerfile` にもほぼ同じ (`sudo` を除いただけの) コマンドを記述しており，二重管理になっていた．Nix を用いれば依存関係の解決が簡単に行なえるし，かつ [`dockerTools`](https://nixos.org/manual/nixpkgs/unstable/#sec-pkgs-dockerTools) を利用すればビルドプロセスをそのまま Docker イメージの作成にも流用できるため，Nix で Chromium をインストールしてビルドに用いることにした．

## The first Nix file

まず以下のような Nix ファイルを用意した．

```nix filename=default.nix
{ pkgs ? import <nixpkgs> {} }:

let
  packageJSON = pkgs.lib.importJSON ./package.json;
  version = packageJSON.version;
in

pkgs.stdenv.mkDerivation {
  name = "herpism-storybook";
  inherit version;

  src = pkgs.nix-gitignore.gitignoreSource [] ./.;

  buildInputs = [
    pkgs.chromium
    pkgs.noto-fonts
    pkgs.yarn
  ];

  PUPPETEER_EXECUTABLE_PATH = "${pkgs.chromium}/bin/chromium";

  buildPhase = ''
    HOME=$TMP yarn install --frozen-lockfile
    yarn build-storybook -o ./dist
    yarn sb extract ./dist
  '';

  installPhase = ''
    mv ./dist $out
  '';
}
```

しかし，これでは `$ yarn sb extract ./dist` を実行した際に失敗してしまう．

## 効かない `$PUPPETEER_EXECUTABLE_PATH`

本題から逸れるが，実は Storybook が環境変数 `$PUPPETEER_EXECUTABLE_PATH` を読んでくれないという問題がある．このため，Puppeteer 自身が勝手に Chromium をインストールしようとしては失敗するという自体に直面した．

これは `@storybook/api` が内部的に [`puppeteer`](https://www.npmjs.com/package/puppeteer) ではなく [`puppeteer-core`](https://www.npmjs.com/package/puppeteer) を用いており，`puppeteer-core` は `puppeteer` とは異なり `$PUPPETEER_EXECUTABLE_PATH` を考慮しないことに起因する．また，残念なことに，Storybook の CLI にも Chromium のパスを指定するオプションなどは特に用意されていないため，現状では自前で用意した Chromium を使用することができなくなってしまっている．

この問題に対処するため，以下のコマンドを build phase に追加した．

```diff filename=default.nix
@@ -22,6 +22,8 @@ pkgs.stdenv.mkDerivation {
   buildPhase = ''
     HOME=$TMP yarn install --frozen-lockfile
     yarn build-storybook -o ./dist
+    yarn add -D puppeteer
+    sed -i ./node_modules/@storybook/cli/dist/extract.js -e s/puppeteer-core/puppeteer/
     yarn sb extract ./dist
   '';
```

また，この件に関しては GitHub に issue を立てておいた．
https://github.com/storybookjs/storybook/issues/13724

## 立ち上がらない Chromium

話を本筋に戻す．上記の問題を解決し，自前で用意した Chromium を用いるように Puppeteer に伝えても，`ECONNRESET` が発生してしまい，失敗してしまう．どうやら Chromium がうまく立ち上がっていないようである．

普段 CI 上で Nix によるビルドを行いたい場合には，Docker イメージとして [`nixos/nix`](https://hub.docker.com/r/nixos/nix) を用いるのだが，これは Alpine Linux のイメージを継承しているため，`apk` コマンドが利用できる．CircleCI に SSH で接続し，デバッグを試みていたところ，おもしろいことに `$ apk add chromium` で Chromium をインストールしておきさえすれば，**Puppeteer が `apk` でインストールした Chromium を使わずとも**実行に成功することがわかった．`apk` を用いて Chromium をインストールする際には100を超える依存が同時にインストールされるので，そのいずれかによって生じる副作用によって何らかの変化が起こっていると考えた．インストールされたパケッジのリストを抽出し，問題の切り分けを行ったところ，`fontconfig` が犯人であると判明した．`fontconfig` のインストール時に `/etc/fonts/fonts.conf` が作成されることが原因だったようである．

## `$FONTCONFIG_FILE` を指定する

Nix で `fontconfig` をインストールした際には，`/etc/` 以下にファイルを作成するなどといった行儀の悪いことは行われない．`fonts.conf` へのパスは環境変数 `$FONTCONFIG_FILE` によって設定でき，Chromium もこれを考慮してくれるようだったので，Nix ファイルに以下のような変更を加えた．

```diff filename=default.nix
@@ -13,10 +13,12 @@ pkgs.stdenv.mkDerivation {

   buildInputs = [
     pkgs.chromium
+    pkgs.fontconfig
     pkgs.noto-fonts
     pkgs.yarn
   ];

+  FONTCONFIG_FILE = "${pkgs.fontconfig.out}/etc/fonts/fonts.conf";
   PUPPETEER_EXECUTABLE_PATH = "${pkgs.chromium}/bin/chromium";

   buildPhase = ''
```

こうして無事に Nix を用いて `metadata.json` を伴った静的な Storybook をビルドすることができた．

## コード全文

```nix filename=default.nix
{ pkgs ? import <nixpkgs> {} }:

let
  packageJSON = pkgs.lib.importJSON ./package.json;
  version = packageJSON.version;
in

pkgs.stdenv.mkDerivation {
  name = "herpism-storybook";
  inherit version;

  src = pkgs.nix-gitignore.gitignoreSource [] ./.;

  buildInputs = [
    pkgs.chromium
    pkgs.fontconfig
    pkgs.noto-fonts
    pkgs.yarn
  ];

  FONTCONFIG_FILE = "${pkgs.fontconfig.out}/etc/fonts/fonts.conf";
  PUPPETEER_EXECUTABLE_PATH = "${pkgs.chromium}/bin/chromium";

  buildPhase = ''
    HOME=$TMP yarn install --frozen-lockfile
    yarn build-storybook -o ./dist
    yarn add -D puppeteer
    sed -i ./node_modules/@storybook/cli/dist/extract.js -e s/puppeteer-core/puppeteer/
    yarn sb extract ./dist
  '';

  installPhase = ''
    mv ./dist $out
  '';
}
```
