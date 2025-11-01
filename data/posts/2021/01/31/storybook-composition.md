---
keywords:
  - Storybook
---

# Storybookの中から別のStorybookを参照できるStorybook compositionを試してみる

Storybook 6.0 から，あるStorybookの中から別のStorybookを参照することができる"Storybook composition"という機能が導入された．

https://storybook.js.org/docs/html/workflows/storybook-composition

これは目玉機能として挙げられているものの，2021年1月現在この機能についてのドキュメンテーションが十分になされていない．そのため，不足している情報を補完することを目的としてこの記事を書くことにした．

---

## モチベーション

[HERP](https://herp.co.jp/)では`herpism`と呼ばれる社内用UIコンポーネントライブラリを管理しており，プライベートなnpmパケッジとして配布している．また`herpism`のStorybookは，社内のメンバのみがアクセス可能な環境にホスティングされている．

各フロントエンドのプロジェクトは`herpism`に依存している．

```plaintext
                               +---------------------+
                          +----|      Project A      |
                          |    +---------------------+
+-------------------+     |    +---------------------+
|      herpism      |<----+----|      Project B      |
+-------------------+     |    +---------------------+
                          |    +---------------------+
                          +----|      Project C      |
                               +---------------------+
```

またそれぞれのプロジェクトは，当該プロジェクト内でのみ用いられるUIコンポーネントを掲載したStorybookを持っている．

```plaintext
                               +---------------------+
                               | Project A Storybook |
                               +---------------------+
+-------------------+          +---------------------+
| herpism Storybook |          | Project B Storybook |
+-------------------+          +---------------------+
                               +---------------------+
                               | Project C Storybook |
                               +---------------------+
```

このような構成を取っているため，各プロジェクトのStorybookから，`herpism`に存在するUIコンポーネントを確認できると有用である．

```plaintext
                               +---------------------+
                          +----| Project A Storybook |
                          |    +---------------------+
+-------------------+     |    +---------------------+
| herpism Storybook |<----+----| Project B Storybook |
+-------------------+     |    +---------------------+
                          |    +---------------------+
                          +----| Project C Storybook |
                               +---------------------+
```

以下では，UIコンポーネントライブラリ(図中左側)を**参照先**，ライブラリを用いるプロジェクト(図中右側)を**参照元**と呼称する．また，参照元のStorybookはlocalhostで閲覧されることを前提している．

## ドキュメント

compositionについてのドキュメントとしては，以下の2ページが存在する．

https://storybook.js.org/docs/html/workflows/storybook-composition

[](https://storybook.js.org/docs/html/workflows/package-composition)

前者は参照元の`.storybook/main.js`に設定を書く手法であり，後者は参照先のパケッジの`package.json`に設定を書いておくことで，参照元のStorybookに自動的に読み込ませる手法である．個別のプロジェクトでの設定の手間を省くことができるため，今回は"package composition"と呼ばれている後者の手法を採用する．

package compositionを実現するためには，以下の手順を踏む必要がある．

- `package.json`に`storybook`フィールドを追加する
- `stories.json`を配置する
- `metadata.json`を配置する（任意）
- CORSに関する設定を行う

以下で`https://storybook.example.com`で参照先のStorybookがホスティングされていることを前提する．

## `package.json`に`storybook`フィールドを追加する

コンポーネントライブラリの`package.json`に，StorybookがホスティングされているURLを以下のように記載する．

```diff filename=package.json
@@ -1,4 +1,7 @@
 {
   "name": "herpism",
-  "version": "4.6.0"
+  "version": "4.6.0",
+  "storybook": {
+    "url": "https://storybook.example.com"
+  }
 }
```

このような`package.json`を持つパケッジが`node_modules/`配下に存在するプロジェクトでStorybookを立ち上げると，自動的に`https://storybook.example.com`でホストされているStorybookを埋め込もうとしてくれる．

## `stories.json`を配置する

Storybook内に存在するコンポーネントの情報を記載した`stories.json`というファイルが配信されている必要がある．[`@storybook/cli`](https://www.npmjs.com/package/@storybook/cli)には`stories.json`を生成するための`$ sb extract`というサブコマンドが用意されており，`$ npx build-storybook`で静的なStorybookをビルドした後，そのディレクトリを引数に与えて実行すると，ディレクトリ内に`stories.json`を生成してくれる．これは[`puppeteer`](https://www.npmjs.com/package/puppeteer)を通じてヘッドレスモードのChromiumが起動することで実現されている．

```sh
$ npx build-storybook -o ./dist
$ npx sb extract ./dist
```

うまくいけば以下のような内容をもつ`./dist/stories.json`が生成される．公式ドキュメントには以下のようなJSONが記載されている．

```json filename=dist/stories.json
{
  "v": 2,
  "globalParameters": {},
  "kindParameters": {
    "components/myComponent": {
      "fileName": 445,
      "framework": "react"
    },
    "components/myOtherComponent": {
      "fileName": 447,
      "framework": "react"
    }
  },
  "stories": {
    "components-mycomponent--simple": {
      "id": "components-mycomponent--simple",
      "name": "Simple",
      "kind": "components/myComponent",
      "story": "Simple",
      "parameters": {
        "__id": "components-mycomponent--simple",
        "__isArgsStory": true
      }
    },
    "components-myothercomponent--simple": {
      "id": "components-myothercomponent--simple",
      "name": "Simple",
      "kind": "components/myothercomponent",
      "story": "Simple",
      "parameters": {
        "__id": "components-myothercomponent--simple",
        "__isArgsStory": true
      }
    }
  }
}
```

## `metadata.json`を配置する（任意）

公式ドキュメントには，compositionのためには`metadata.json`なるファイルが必要だと記載されているが，このファイルがどのようなものであるかについては言及されていない．加えて，このファイルの存在は必須ではなく任意である．[GitHubのissue comment](https://github.com/storybookjs/storybook/issues/12202#issuecomment-678864851)を参考にすると，以下のような内容であるべきだということがわかる．

```json filename=dist/metadata.json
{
  "versions": {
    "v4.6.0": "https://storybook.example.com"
  }
}
```

`versions`フィールドに複数のヴァージョンを記載し，異なるヴァージョンに対して異なるURLを割り当てておけば，参照元のStorybookからヴァージョンを切り替えることもできる．しかしHERPでの事例では，過去のヴァージョンのStorybookを半永久的にホスティングし続けるのが面倒なので，単一のヴァージョンのみを記載している．このようなJSONを生成する適当なスクリプトを用意しておき，ビルドプロセスに組み込んでおくのがいいだろう．

## CORSに関する設定を行う

Storybook compositionでは，参照元のStorybookを開いているブラウザから[Cross-Origin Resource Sharing](https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS) (CORS)を用いて外部のリソースを読み込むため，参照先のStorybookをホストしているサーヴァ側で，CORSを許可するよう設定を行っておく必要がある．

HERPではStorybookを配信するDocker imageを，[Istio](https://istio.io/)が入ったKubernetesクラスタにデプロイしているため，[`VirtualService`](https://istio.io/latest/docs/reference/config/networking/virtual-service/)に以下のような[`CorsPolicy`](https://istio.io/latest/docs/reference/config/networking/virtual-service/#CorsPolicy)を追加することで対応した．また，[`oauth2-proxy`](https://oauth2-proxy.github.io/oauth2-proxy/)を用いたアクセス制限を施しているため，`allowCredentials: true`を指定している．

```yaml
corsPolicy:
  # Access-Control-Allow-Credentialsヘッダに対応
  allowCredentials: true

  # Access-Control-Allow-Methodsヘッダに対応
  allowMethods:
    - GET

  # Access-Control-Allow-Originヘッダに対応
  allowOrigins:
    - regex: http://localhost:[0-9]+

  # Access-Control-Max-Ageヘッダに対応
  maxAge: 24h
```

前段に認証を挟む場合，`Set-Cookie`ヘッダに`SameSite=None`を指定することをお忘れなきよう．

設定がうまくいっていれば，参照元のStorybookを立ち上げた際に，サイドバーの下部から参照先のStorybookを参照できるようになっているはずである．

![Referenced Storybook](https://gyazo.com/05f20860f62f6bee2de9920ce52ef7ed.png)
