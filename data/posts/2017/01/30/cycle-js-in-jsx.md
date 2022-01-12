# Cycle.js を JSX で書く

Cycle.js と DOM Driver で Web アプリケーションを書く際に，sink に流す仮想 DOM を JSX で記述したい．公式を読むと普通にやり方が書いてあったが，ググって解決しようとしていると，古い手順に当たってしまい，少し引っかかってしまった．加えて，「最近のフロントエンドのエコシステムはこういう感じなのか」と思うところもあったので，同じような立場の人のために記録を残しておくことにする．なお，同じ轍を踏まぬよう，読者諸賢におかれましては，あくまで2017年1月30日現在の情報であることにご注意願いたい．

以下では，Cycle.js (w/ xstream) でウェブアプリケーションを構築する．また，アセットのバンドルには Webpack を用い，`babel-loader` を通じて ES2015+ および JSX のトランスパイルを行うこととする．

なお，今回のプロジェクトは GItHub 上で公開しているので参考にされたい．

[](https://github.com/ryota-ka/cycle-jsx-example)

---

`create-cycle-app` を使ってもいいが，より深く理解するため，スクラッチでプロジェクトを作ることにする．

とりあえずプロジェクトの初期化を行う．

```sh
$ npm init
# 雑なプロジェクトの場合は `package.json` に `{ "private": true }` とだけ書いておけばいいと思う．
```

続いて，必要なパッケージをインストールする．

```sh
$ yarn add @cycle/xstream-run @cycle/dom@^14.3.0 xstream
$ yarn add -D babel-core babel-loader babel-plugin-transform-react-jsx babel-preset-latest snabbdom-jsx webpack
```

1行目の dependencies が，アプリケーションを動かすために必要なパッケージで，2行目の dev dependencies は，記述したスクリプトをブラウザが解釈できるようなコードにトランスパイルするために必要なパッケージである．

なお，本稿執筆時点では，`@cycle/dom` を普通に `yarn add` で 入れると 15.0.0-rc1 が入るが，このバージョンを用いると，webpack でビルドする際に，以下のようなエラーが発生するので，使用するバージョンを陽に指定してある．

```sh
ERROR in ./~/@cycle/dom/lib/MainDOMSource.js
Module not found: Error: Cannot resolve module '@cycle/run/lib/adapt' in /Users/Ryota/dev/cycle-jsx-example/node_modules/@cycle/dom/lib
 @ ./~/@cycle/dom/lib/MainDOMSource.js 2:14-45

ERROR in ./~/@cycle/dom/lib/HTMLSource.js
Module not found: Error: Cannot resolve module '@cycle/run/lib/adapt' in /Users/Ryota/dev/cycle-jsx-example/node_modules/@cycle/dom/lib
 @ ./~/@cycle/dom/lib/HTMLSource.js 3:14-45

ERROR in ./~/@cycle/dom/lib/mockDOMSource.js
Module not found: Error: Cannot resolve module '@cycle/run/lib/adapt' in /Users/Ryota/dev/cycle-jsx-example/node_modules/@cycle/dom/lib
 @ ./~/@cycle/dom/lib/mockDOMSource.js 3:14-45

ERROR in ./~/@cycle/dom/lib/DocumentDOMSource.js
Module not found: Error: Cannot resolve module '@cycle/run/lib/adapt' in /Users/Ryota/dev/cycle-jsx-example/node_modules/@cycle/dom/lib
 @ ./~/@cycle/dom/lib/DocumentDOMSource.js 3:14-45

ERROR in ./~/@cycle/dom/lib/BodyDOMSource.js
Module not found: Error: Cannot resolve module '@cycle/run/lib/adapt' in /Users/Ryota/dev/cycle-jsx-example/node_modules/@cycle/dom/lib
 @ ./~/@cycle/dom/lib/BodyDOMSource.js 3:14-45
```

次に，`webpack.config.js` を記述する．

```javascript
module.exports = {
  entry: {
    app: './src/app.js',
  },
  output: {
    filename: '[name].js',
    path: './dest',
  },
  module: {
    loaders: [
      {
        exclude: /node_modules/,
        loader: 'babel',
        query: {
          plugins: [['transform-react-jsx', { pragma: 'html' }]],
          presets: ['latest'],
        },
        test: /\.js$/,
      },
    ],
  },
};
```

`src/app.js` を読み込み，Babel を挟んで `dest/app.js` に出力している．

preset には [`latest`](https://babeljs.io/docs/plugins/preset-latest/) を指定しておくと，`es2015`, `es2016`, `es2017` が含まれていて便利である．実際，Babel 公式も "All you need to compile what's in ES2015+" と謳っているくらいだから，使用が推奨されているのだと思う (個人の見解です)．

プラグインとして `transform-react-jsx` を噛ませていて，こいつが JSX を JavaScript に変換してくれる．プラグインの名前に "react" と入っているくらいだから，デフォルトでは `React.createElement` に変換するのだが，オプションとして pragma を指定することで，好きな関数を選ぶことができる．今回は `snabbdom-jsx` の `html` 関数を指定している．[Snabbdom](https://github.com/snabbdom/snabbdom) はシンプルさ・高い性能・高い拡張性を掲げている仮想 DOM ライブラリで，どうも最近アツいらしい．

古い情報だと，`@cycle/dom` から `hJSX` という関数を import して，これを使えという風に書いていたが，どうもこれは outdated らしい．実際，@cycle/dom@14.3.0 では `hJSX` は export されていなかった．

上記の `webpack.config.js` を用意しておけば，以下のコマンドで，ファイルを監視しつつリアルタイムでビルドしてくれる．

```sh
$(npm bin)/webpack --watch
```

とはいえ，まだ `src/app.js` がないので，実際にアプリケーションのコードを書く．

```javascript
import xs from 'xstream';
import { run } from '@cycle/xstream-run';
import { makeDOMDriver } from '@cycle/dom';
import { html } from 'snabbdom-jsx';

function main({ DOM }) {
  return {
    DOM: xs.of(
      <div>
        <h1>Hello, world!</h1>
      </div>,
    ),
  };
}

run(main, {
  DOM: makeDOMDriver('#app'),
});
```

前述の通り，JSX は `html` 関数に変換されるため，`snabbdom-jsx` から import しておく．HTML から `dest/app.js` を読み込めば，`<div id="app">` の要素が置換されて，"Hello, world!" と表示されるはずである．
