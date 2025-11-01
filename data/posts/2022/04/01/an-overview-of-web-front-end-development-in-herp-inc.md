---
keywords:
  - Cycle.js
  - fp-ts
  - fp-ts-routing
  - OpenAPI Generator
---

# HERPにおけるWebフロントエンド開発概観（2022年春編）

## はじめに

この記事は，[HERP](https://herp.co.jp/)におけるWebフロントエンド開発の概観を，世間の開発者に——特に潜在的・顕在的な候補者の方に——知ってもらうことを目的として書かれた．HERPでは現在Webフロントエンドエンジニアを積極的に募集しているが，仮に入社したとしてどのような仕事をすることになるかのイメージが付いた方が，検討の候補に入れてもらいやすいのではないかという目論見による．また，採用している技術スタックにも珍しいものがあるため，単純に読み物として楽しめるかもしれない．なお，開発の実情について知ってもらうのが目的であり，実装の良し悪しについて議論することは目的としていない．

---

## HERP でのアプリケーション開発

B2B SaaSとして，主にITスタートアップ企業向けの，採用管理システムおよびタレントプールシステムを開発・提供している．

https://lp.herp.cloud/

https://lp.herp.cloud/nurture/

そもそもプロダクトを通じて何を実現したいかについては，HERP Culture Deckの以下の節を参照されたい．

https://culture.herp.co.jp/#9bb1b98bba3643bfb464bc5eac254525

ソフトウェア開発者にとって，開発しているプロダクトや，開発プロセスに関する情報もまた重要であるとは重々承知している．しかし，それぞれ独立した記事が書けてしまうほどに大きな話題なので，この記事では大胆にも割愛し，私以外の開発者が筆を執る機会に譲ることにする．

## ルーティング

クライアントサイドアプリケーションは完全なSPAにはしておらず，「ページ」と呼ばれる単位で分割された小さいSPAの集合体になっている．クライアントサイドのエントリポイントでルーティングを行っており，リクエスト時の`location.pathname`の値に応じて，dynamic importされるcomponentが決定される．ルーティング自体は[`fp-ts-routing`](https://github.com/gcanti/fp-ts-routing)を用いて行っている．

```typescript
import * as Alt from 'fp-ts/Alternative';
import { end, lit, parse, parser, Route, str } from 'fp-ts-routing';

/**
 * ルートの定義
 *
 * アンカー要素の `href` 属性の値や History API でも，このようにして定義した値を利用している．
 */
const routes = {
  /**
   * `/p/resources`
   */
  ListResources: lit('p').then(lit('resources')).then(end),
  /**
   * `/p/resources/id/:id`
   */
  ShowResource: lit('p').then(lit('resources')).then(lit('id')).then(str('id')).then(end),
};

/**
 * ルーティングの定義
 */
const routing = Alt.altAll<Promise<Page>>(parser)([
  routes.ListResources.parser.map(() => import('./ListResources')),
  routes.ShowResource.parser.map(() => import('./ShowResource')),
]);

/**
 * `location.pathname`を受け取り，対応するcomponentを返す関数
 */
async function router(pathname: string): Promise<PageFn> {
  const route = Route.parse(pathname);
  const { component } = await parse(routing, route, import('./NotFound'));

  return component;
}
```

## Cycle.js

Cycle.jsは，主にクライアントサイドアプリケーションの開発のための，stream（あるいはObservable[^1]）として知られる概念の取り扱いに指針を与えるフレームワークである．Cycle.jsのコア部分は極めて小さく，実のところ，副作用と非同期処理に関する以下のような抽象化機構を提供しているに過ぎない．

Cycle.jsで記述されたアプリケーションは次のように動く．

アプリケーションは，自身が発生させたい副作用を表現する値をstreamに流す．副作用の例としては以下のようなものがある．

- 画面を再描画したいときに仮想DOMを流す
- バックエンドと通信したいときにAPIリクエストを流す
- アプリケーションのstateを更新したいときにreducerを流す

その他，開発者が副作用だと思いたいもの[^2]を切り分けて扱うことができる．

streamに流された値は，"driver"と呼ばれる，実際に副作用を発生させる層によって解決される．例えばDOM driverは，アプリケーションから受け取った仮想DOMを元に画面を再描画し，ブラウザ上でユーザが発生させたDOM eventをアプリケーションに通知する．例えばAPI driverは，アプリケーションから受け取った値を元にAPIリクエストを送信し，返却された API レスポンスをアプリケーションに通知する．

以下に掲げるのは，[公式サイト](https://cycle.js.org/)から引用した模式図である．

![How Cycle.js application works](https://cycle.js.org/img/cycle-nested-frontpage.svg)

これらの値のやり取りはすべてstreamを通じて行われる．streamを扱うということは，別の見方をすれば，後続の計算を陽に扱うということでもある．`main()`関数は直接副作用を発生させるわけではなく，あくまで副作用を表現する値を流すだけなので，レンダリングの最適化や，不要なAPIリクエストを間引くといった操作も，streamの変換を通じて実現される．

図中の`main()`のような関数をCycle.jsでは"component"と呼ぶ．Cycle.jsの"component"はReactやVue.jsのそれとはまったく異なる概念であることに注意されたい．componentは，"sources"と呼ばれるstreamの束を受け取り，"sinks"と呼ばれるstreamを返す，単なる関数である．

```typescript
const main = ({ API, DOM, state }: Sources): Sinks => {
  // 外部からの入力を元にsinkに流すstreamを作成する

  return {
    API: apiRequest$,
    DOM: view$,
    state: reducer$,
    // その他開発者が副作用だと思いたいもの
  };
};
```

### state 管理

[`@cycle/state`](https://cycle.js.org/api/state.html)という公式のstate管理ライブラリがあり，これを利用している．

`@cycle/state`の仕組みは，Reduxと対比すると理解しやすい．[Reduxが掲げる3つの原則](https://redux.js.org/understanding/thinking-in-redux/three-principles)である"Single source of truth"[^3], "State is read-only", "Changes are made with pure functions" は`@cycle/state`にもそのまま当てはまる．

Reduxではstateを変更するためにactionをdispatchし，reducerは `(state: State, action: Action) => State`という型を持つ．一方`@cycle/state`では，ユーザの入力やAPIレスポンスなどのstreamを，あらかじめ`Stream<(state: State) => State>`というstreamに変換しておき，`state` sinkに流す．実際，Reduxスタイルのreducerにカリー化を施すと`(action: Action) => (state: State) => State`という関数が得られる．

Reduxの`combineReducers`のように，stateのうち特定のpropertyのみを切り出し，その管理を子componentに移譲する形で分割統治を行うこともできる[^4]．

```typescript
type State = {
  potato: PotateState; // 子componentはこのpropertyにのみアクセスできる
  tomato: TomatoState;
};
```

しかし，`combineReducers`とは違い，親componentのstateを元にした子componentのstateの導出は，何も標準射影に限らない．stateの導出・更新ロジックに手を加えたいケースでは，"lens"と呼ばれる概念が利用できる．lensは以下のようなgetterとsetterの組である．

```typescript
type Lens<Parent, Child> = {
  /**
   * 親のstateから子のstateを導出する
   */
  get: (parent: Parent) => Child;
  /**
   * 子のstateの更新を親のstateに反映する
   */
  set: (parent: Parent, child: Child) => Parent;
};
```

stateをimmutableに書き換えるコードはしばしば煩雑になりがちなので，[Immer](https://immerjs.github.io/immer/)も合わせて利用している．

### view

[Flux](https://facebook.github.io/flux/)アーキテクチャの台頭により，「viewはstateの影である」という価値観は，現代では広く認識されているように思う．もちろん我々のアプリケーションにおいても例外ではない．view層は，アプリケーションのstateを受け取って仮想DOMを返す，純粋な関数として実装される．

Cycle.jsはすべてがpluggableな設計になっているので，view層に利用するライブラリも切り替えられる[^5]のだが，[Snabbdom](https://github.com/snabbdom/snabbdom)がデファクトスタンダードであり，我々もこれを採用している．SnabbdomはVue.jsのfork元としても知られる，軽量かつ拡張性に富んだ仮想DOMライブラリである．

view層は受け取ったデータを仮想DOMに変換する役割のみを担い，状態やロジックを持たない[^6]純粋な関数から構成される．このような関数は，Cycle.jsのcomponentと区別するために"UI component"と呼んで区別している．[Storybook](https://storybook.js.org/)にはこの関数を単位として収録するようにしている．

ボタンやチェックボックス，テキストフィールドといった，複数のプロジェクト間で共有したいUI componentは，`herpism`と呼ばれる社内ライブラリに切り出されている．しかしながら，小さなUI componentは比較的よく再利用されているものの，それより大きな，例えばレイアウトパターンといったものはあまり再利用できていない．先月，待望のシニアデザイナが入社してくれたということもあり，開発・機能提供速度の向上を狙って今後注力していきたい領域であると考えている．

開発の最初期こそ，コミュニティの流儀に則りhyperscript helper functionsによる記述を行っていた．しかし，ただの関数というのは自由度が高すぎたようで，開発を進める中で統一されたインタフェースの必要性を感じ，早い段階からJSXに切り替えている．以前のSnabbdomはJSX pragmaを提供していなかったため，サードパーティ製のライブラリを利用していたが，昨年ライブラリを内製し[^7]，既にそちらに移行している．

### バックエンドアプリケーションとの通信

伝統的に，1つのフロントエンドアプリケーションに対し，1つのバックエンドアプリケーションが対応する形での開発を進めてきた．フロントエンドアプリケーションは，それぞれ対応するバックエンドアプリケーションとの通信を行うために，どのようなAPIが提供されているのかを知る必要がある．

古めのバックエンドアプリケーションたちは，[OpenAPI Specification 2.0](https://swagger.io/specification/v2/)[^8]で記述されたAPI specを提供している[^9]．Cycle.jsで利用できるdriverを生成するため，Groovyで書かれたコード生成スクリプトを用意し，[mustache](https://mustache.github.io/)で記述されたテンプレートからコード生成を行っている．コード生成のためのスクリプトやテンプレートがGit submoduleで共有されていたり，sum typeを扱うための独自拡張（vendor extensions）が実装されていたりと，あまり健康な状態ではない．

一部のリポジトリでは，上記の仕組みを[OpenAPI Specification 3.0](https://swagger.io/specification/)ベースに移行した．下敷きとしている[JSON Schema](https://json-schema.org/)のdraftが新しいため，sum typeを表現するのに[`oneOf`](https://json-schema.org/understanding-json-schema/reference/combining.html#oneof)といった便利な機能を素直に利用できる．また，受け取ったJSONの形状を検証するため[^10]，[`io-ts`](https://gcanti.github.io/io-ts/)を利用している．

ありがちな問題かもしれないが，どちらもAPIクライアントをclassで実装しているので，code splittingにあたり不利に働いてしまっている．しかし，出来合いのgeneratorを利用せず，自前でテンプレートを用意するという手法を取っているため，必要に迫られた際にもこのあたりの調整はしやすいだろうと考えている．

Groovyによる生成スクリプトは概ね以下のようになっている．`@Grab`の1行で，必要なパッケージを実行時にダウンロードしてきてくれるのは，とりわけこういった用途ではなかなか体験がよい．

```groovy
@Grab(group = 'io.swagger.core.v3', module = 'swagger-models', version = '2.1.10')
import io.swagger.v3.oas.models.media.Schema

@Grab(group = 'org.openapitools', module = 'openapi-generator-cli', version = '5.2.1')
import org.openapitools.codegen.*
import org.openapitools.codegen.languages.*

class APIDriverCodegen extends AbstractTypeScriptClientCodegen {
  String name = 'APIDriverCodegen'

  Boolean supportsES6 = true
  Boolean useSingleRequestParameter = true

  static main(String[] args) {
    OpenAPIGenerator.main(args)
  }

  @Override
  void processOpts() {
    super.processOpts()

    supportingFiles.add(new SupportingFile('APIDriver.mustache', '', 'APIDriver.ts'))
  }
}
```

章の冒頭で「伝統的に，1つのフロントエンドアプリケーションに対し，1つのバックエンドアプリケーションが対応する形での開発を進めてきた」と書いたが，バックエンドエンジニアおよびアプリケーションが増えるに従い，この形態での開発が全体のシステムアーキテクチャにそぐわなくなってきている．そのため最近では，データ取得を抽象化するレイヤとしてGraphQLを導入しつつある．クライアントサイドアプリケーションでは[Apollo Client](https://www.apollographql.com/docs/react)を利用しているが，内部的にObservable ([`zen-observable`](https://www.npmjs.com/package/zen-observable))が使われていることから，我々のアプリケーションアーキテクチャとの親和性も高く，簡単なdriverを書くことでCycle.jsによく統合されている．また[GraphQL Code Generator](https://www.graphql-code-generator.com/)も合わせて利用している．

## `fp-ts`

既に`fp-ts-routing`および`io-ts`には言及したが，[`fp-ts`](https://gcanti.github.io/fp-ts/)およびそのエコシステムはよく利用されている．と言っても，よく使われるmoduleは限られており，それほど多くはない．代表的なものを以下に挙げる．

- [`Option`](https://gcanti.github.io/fp-ts/modules/Option.ts.html): 存在しないかもしれない値に用いている．optional chainingとnullish coalescing operatorの登場により重要性は下がったが，構造に対してmapすることができるという優位性は未だに残っている．
- [`Ord`](https://gcanti.github.io/fp-ts/modules/Ord.ts.html): 画面に表示されるリソースに順序関係を与えるために用いている．
- [`Map`](https://gcanti.github.io/fp-ts/modules/Map.ts.html)および[`Set`](https://gcanti.github.io/fp-ts/modules/Set.ts.html): コレクションのimmutableな操作に用いている．

## CSS

創業以来alt CSSとして [Stylus](https://stylus-lang.com/)を利用してきている．将来性に不安を感じるので，どこかのタイミングで乗り換えを検討することになるかもしれない．しかし喫緊の課題というわけでもないので，少なくとも向こう数年は付き合い続けることになるだろう．

長きに亘ってBEMによる命名を行ってきたが，conventionによる運用に限界と無意味さを感じたので，昨年の初頭にCSS modulesを導入した．仮に将来Stylusを脱却するという選択肢を採った際，ファイルごとの段階的な移行を行いやすいのではないかという目論見もある．とはいえ，当然Stylusに固有の資産[^11]もあるため，それぞれのファイルは依然Stylusで記述されている．そのため，`*.module.styl`ファイルにはまず[`stylus-loader`](https://www.npmjs.com/package/stylus-loader)が適用され，次に[`css-loader`](https://webpack.js.org/loaders/css-loader/)が適用される形になっている．また，`*.module.styl`ファイルのTypeScriptの型定義を生成するために[`css-modules-typescript-loader`](https://www.npmjs.com/package/css-modules-typescript-loader)も合わせて利用している．

## BFF

前提として，HERPでのアプリケーションは，[Lambda](https://aws.amazon.com/lambda/) functionなどの一部の例外を除き，すべて[Amazon EKS](https://aws.amazon.com/eks/)で構築されたKubernetesクラスタ上で稼働している．フロントエンドのアプリケーションも例外ではなく，リポジトリは成果物としてDocker imageを作成し，コンテナ化されたアプリケーションがKubernetesクラスタにデプロイされる．

Dockerコンテナの中では，[Koa.js](https://koajs.com/)製のアプリケーションがリクエストを待ち受けている．認証が必要なパスにリクエストが来た場合，Cookie内の情報を用いて認証サーヴィスに問い合わせ，認証を行う．認証が済むと，クライアントに対してHTMLを返す．このHTMLには，認証されたユーザに関する情報や，環境変数経由で設定された種々の値が埋め込まれる．このようなHTMLは，[HTML Webpack Plugin](https://webpack.js.org/plugins/html-webpack-plugin/)が生成したHTMLファイルを元に，リクエスト毎に[`parse5-html-rewriting-stream`](https://www.npmjs.com/package/parse5-html-rewriting-stream)による変換を行って生成されている．

BFFにはまた，GraphQLのresolverも実装されている．[`apollo-server-koa`](https://www.npmjs.com/package/apollo-server-koa)を利用しており，クエリされたリソースに応じて，gRPCなどを通じ他のサーヴィスからデータを取得する．

## 開発環境

ローカルでの開発には[webpack dev server](https://github.com/webpack/webpack-dev-server)を利用している．お世辞にも軽快とは言えないので，最近一部のリポジトリで[Vite](https://vitejs.dev/)を実験的に導入している．

バックエンドアプリケーションや，それらが依存するミドルウェア（e.g. MySQL）は[Docker Compose](https://docs.docker.com/compose/)で立ち上げている．`/api/`以下へのリクエストはwebpack dev serverによってバックエンドアプリケーションにプロキシされる．

開発に必要なランタイム（例えば特定のヴァージョンのNode.js）などの依存は[Nix](https://nixos.org/)を通じて提供されるため，必要なものは各リポジトリの`shell.nix`に記述されている．参照する[`nixpkgs`](https://github.com/NixOS/nixpkgs/)のrevisionを固定しておけば[^12]，各開発者の環境や成果物となるDocker image内において，確実に同一のヴァージョンに解決することができる．加えて，開発環境では[direnv](https://direnv.net/)の[`use nix`](https://direnv.net/man/direnv-stdlib.1.html#codeuse-nix-code)を利用しているため，プロジェクトのディレクトリに`cd`するだけで，プロジェクトに応じた`node`や`yarn`に`$PATH`が通るようになっている．

## ビルド・CI/CD

クライアントサイドアプリケーションはWebpackによるバンドルを行っている．Docker imageのビルドは，依然`Dockerfile`を利用しているものもあるが，最終的には[Nix の docker-tools](https://github.com/NixOS/nixpkgs/tree/master/pkgs/build-support/docker)を利用する方式に統一したいと考えている．

CIには主に[GitHub Actions](https://github.com/features/actions)を利用しているが，以前から利用していた[CircleCI](https://circleci.com/)も併用している．2022年4月現在GitHub Actionsには，保存したartifactを静的ファイルとして配信し，ブラウザから確認できる機能がない．そのため，StorybookやWebpack Bundle Analyzerの生成など，特にこのような用途で用いたいジョブには，専らCircleCIを利用している．

クラスタへのデプロイには[Argo CD](https://argoproj.github.io/cd/)を利用している．target stateとなる，各アプリケーションの構成を記述した[Kustomize](https://kustomize.io/)のmanifestが置かれたレポジトリがGitHub上に存在し，GitOpsを行っている．アプリケーションのリポジトリでGitのtagを作成し，pushすると，CI上でビルドされたDocker imageがコンテナレジストリにpushされる．また，GitHub上でリリースを作成すると，GitOps用リポジトリに存在するYAMLファイルの`newTag`を更新するpull requestが作成される．

クラスタにはdev, staging, productionという3つの環境が存在する．dev環境には，毎晩`master`ブランチ上で作成されるnightly releaseなどがデプロイされる．staging環境は動作検証に利用しており，週に一度程度開発者が作成するstable releaseがデプロイされる．staging環境での動作検証が済んだものは順次production環境にデプロイされる．

## エラー監視・ロギング・メトリクス

クライアントサイドアプリケーションでは，[Datadog](https://www.datadoghq.com/)の[Real User Monitoring](https://www.datadoghq.com/product/real-user-monitoring/)と[Browser Log Collection](https://docs.datadoghq.com/logs/log_collection/javascript/)を利用している．エラートラッキングには[Sentry](https://sentry.io/)を利用しているが，中長期的にはDatadogに統合できるならば望ましいと考えている．

BFFはクラスタ上で動作するNode.js製のアプリケーションだが，クラスタ上のDatadogエージェントがログやメトリクスを自動的に収集してくれる．

## 終わりに

この記事では，現在HERPで行われているWebフロントエンド開発の現状について解説した．B2Bビジネスは，B2Cビジネスと比較すると自分自身がユーザになることが少ないため，実際の業務の想像が付きにくい部分もあるのではないかと思う．文量の関係上，プロダクトについての詳細な紹介は叶わなかったが，少しでもHERPでの開発に興味を持ってもらえたり，あるいは何かしら自身の開発に役立ててもらえたならば幸いである．

今後もSaaSビジネスがトレンドとして台頭していくであろうことを考えると，B2BのSaaSビジネスでありがちな開発上の要求・要件を押さえておくことは，キャリア構築上も有利に働くのではないかと考えている．自身の転職活動や自社でのエンジニア採用を通じ，我々が取り組むドメインに少しなりとも課題を感じたことがあるならば，あるいは技術的な側面から少しでも興味を持ってもらえたならば，転職活動の際に検討の俎上に載せていただけると大変嬉しい．

GitHub上で採用資料を公開しているので，こちらも合わせてご覧いただきたい．

https://github.com/herp-inc/engineering-careers

よりHERPに興味がある方は，以下のフォームからカジュアル面談に申し込むことができる．今回の記事に収まらなかった内容や，あるいはより詳細な内容をお話できるだろう．

https://herp.careers/v2/herpinc/f/id/MY7DEmD5y7z3JUxZxTCyWW

もちろん，既に意向が高ければ，以下の求人に直接ご応募いただくこともできる．

https://herp.careers/v1/herpinc/CrMCGkcKbpNA

## 脚注

[^1]: かつてのCycle.jsは専ら[RxJS](https://rxjs.dev/)に依拠したフレームワークだった．しかし，2016年の[Cycle Diversity](https://cycle.js.org/releases.html#cycle-diversity) (`@cycle/core` v7.0.0)リリース移行，RxJS以外にも異なる複数のstream libraryをサポートしており，そのうち[xstream](https://github.com/staltz/xstream)がデファクトスタンダードの選択肢となっている．xstreamはRxJSと比較すると，hot/coldの区別が存在しない，Webアプリケーション開発によく使われる限られたオペレータのみが提供されている，などの簡易化が図られている．
[^2]: History APIや Storage APIとのやり取り，Web Workerとの非同期通信といった，Webの標準的技術に沿ったものはもちろん，モーダルウィンドウやポップアップウィンドウの表示といった，アプリケーションに固有のものも含め，自由に抽象化することができる．
[^3]: もっとも，すべての状態をstate treeに含めることの取り回しの悪さも，現代では広く認知されるようになったところであると感じている．我々のアプリケーションでも，componentごとに持たせたい状態は当然あるし，そのような管理をしている場面も多くある．そのような利用にも問題なく耐えるのが`@cycle/state`の懐の深さかもしれない．
[^4]: これはCycle.jsの"isolation"という仕組みを通じて実現されている．
[^5]: 例えば[`@cycle/react`](https://www.npmjs.com/package/@cycle/react)を使えば，Reactと共に利用することもできる．
[^6]: ただし実DOMのライフサイクルに関するロジックはSnabbdomのレイヤで記述する．
[^7]: [`@herp-inc/snabbdom-jsx`](https://www.npmjs.com/package/@herp-inc/snabbdom-jsx)というnpmパッケージが利用できるようにしている．
[^8]: かつてはSwagger 2.0として知られていた．
[^9]: ちなみに，バックエンドアプリケーションでもOpenAPIを活用している．API specからコード生成を行っているものもあるし，[`servant-openapi3`](https://hackage.haskell.org/package/servant-openapi3)を利用して，Haskellの型定義からAPI specを生成している例もある．
[^10]: もちろん，バックエンドアプリケーションが常にAPI specに準拠したレスポンスを返すのであれば不要な層だが，何か問題が起きたときに確実に検知できる仕組みを入れておくのも悪くはない．
[^11]: mixinや，`@extend`することを前提としたplaceholder selectorなど．
[^12]: 現在のところ[`niv`](https://github.com/nmattia/niv)を利用しているが，[flakes](https://nixos.wiki/wiki/Flakes)がstableになれば，そちらに移行することも検討するだろう．
