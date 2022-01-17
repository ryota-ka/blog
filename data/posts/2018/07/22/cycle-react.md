# @cycle/react を触ってみる

数日前，[@staltz](https://twitter.com/staltz) 氏から Cycle React のリリースがアナウンスされた．

[](https://twitter.com/andrestaltz/status/1019546640582152192)

[`@cycle/react`](https://www.npmjs.com/package/@cycle/react) を含む一連のライブラリ群は，React component を Cycle.js の中で用いることを可能にし，Cycle.js の component を React の中で用いることを可能にする．

---

## モチベーション

氏は [Use React in Cycle.js and vice-versa](https://staltz.com/use-react-in-cyclejs-and-vice-versa.html) という記事で，次のように述べている．

> I have been critical of React, but that’s a normal thing as a framework author: you have different opinions on how frontend should be done. I’ve always been very positive about React Native, though, because it’s a game changer. It allows us to bring the JavaScript ecosystem to mobile development, without compromising (that much) on user experience.

[@hiroqn](https://github.com/hiroqn) に聞いた話だが，Cycle.js で React Native を書くというアイディアは数年前から既にあったらしく，[かつての README](https://github.com/cyclejs/react-native/blob/cb5e1916a90142002040c13970fa25f58307961f/README.md) を読んでみると，CycleConf 2016 のハッカソンで開発されたものが原型らしい．

氏は Cycle React の存在には以下のような意義があるとしている．

- React ユーザにとっては
  - 開発に Cycle.js のアーキテクチャを取り入れることができる
    - `cycle-onionify` のような state management ツールが利用可能になる
    - 様々な driver を使って副作用を管理できるようになる
  - `@cycle/react` によって
    - コンポーネントを
      - `this`-less に書ける
      - `class` を使わずに書ける
      - コード量が少なくなる
      - `const` だけで (immutable に)書ける
    - stream を使った reactive なプログラミングができる
- Cycle.js ユーザにとっては
  - React コミュニティに存在する数多の資産が利用可能になる
  - Cycle.js で記述されたコンポーネントを React ユーザに対しても提供できる
  - React Native を Cycle.js で記述できる

と，React ユーザにとってはあまり嬉しい訳ではないような気がする[^1]が，Cycle.js の側からすると，React コミュニティの資産にアクセスできるようになるのは魅力的である．

## 使い方

通常の Cycle.js アプリケーションとは異なり，`@cycle/dom` の代わりに `@cycle/react` を用いる．加えて，プラットフォームに合わせて `@cycle/react-dom` と `@cycle/react-native` のどちらかを選ぶ．

上記記事中のサンプルコードを見ればわかるが，基本的には JSX は用いずに，`@cycle/react` が export している `h` 関数を用いることを想定している．

React は `handleClick` といった書き方で，イベントが発生した際に関数を proactive に呼び出す一方，Cycle.js は reactive なので，DOM 側はどういったイベントを引き起こしうるのかは知らず，component の中で `sources.DOM.select('.selector').events('click')` といった形でイベントの stream を取得するという方式を取っている．しかしながら，CSS セレクタという概念が存在しない React Native にも対応するためには，別の仕組みを用意するしかないので `@cycle/dom` とは少し事情が異なり，higher-order component で囲むことによってこれを実現するというアプローチを取ったようだ．`sel` を渡すために JSX は使えないので，hyperscript を前提としたインタフェースを採用したらしい．実際記事中にも，`sel` が利用できないことを除けば JSX でも動作するし，将来的に JSX の完全なサポートがなされるかもしれないが，あくまで hyperscript の方が優先，という趣旨の記述がある．現時点で JSX だけで済ませたい場合には，以下のような SFC を用意しておくとなんとかなるが，あまり推奨されるようなものではないだろう．

```typescript
import * as React from 'react';

import { incorporate } from '@cycle/react/lib/cjs/incorporate';

export type Props = {
  sel: string | symbol;
  children: React.ReactNode;
};

function Incorporator<P = any>(props: Props): React.ReactElement<P> {
  const { children, sel } = props;

  return React.createElement<any>(incorporate('div'), { sel }, children);
}

export default Incorporator;
```

```typescript
const handleClick = Symbol();

function View(...) {
    return (
        <Incorporator sel={ handleClick }>
            <Button />
        </Incorporator>
    );
}

function main({ react }) {
    const clickEvent$ = react.select(handleClick).events('click');
    // ...
}
```

## コードサンプル

ここからは実際に `@cycle/react` を用いたアプリケーションを作ってみる．冒頭で述べたとおり，`@cycle/react` を使えば

- React component を Cycle.js 中で利用できる
- Cycle.js の component を React 中で利用できる

という2つの恩恵を得ることができるのだが，今回は前者のみを検証する Cycle.js アプリケーションを作成する．また，`@cycle/react-dom` と `@cycle/react-native` という選択肢があるが，今回は `@cycle/react-dom` を用いる．加えて，せっかく React のエコシステムに乗っかるので，何か React らしいものをと考えた結果，[`styled-components`](https://www.styled-components.com/) を取り入れることにした．

以下のパッケージをインストールする．

```console
$ yarn add @cycle/react @cycle/react-dom @cycle/run cycle-restart react react-dom styled-components xstream
$ yarn add -D @types/react @types/react-dom html-webpack-plugin ts-loader typescript webpack webpack-serve
```

コード(雑)はこんな感じ．

```typescript
// src/components/Greeter.tsx

import * as React from 'react';

import styled from 'styled-components';

export type Props = {
  name: string;
};

const H1 = styled.h1`
  color: green;
  margin-bottom: 20px;
`;

export default function Greeter({ name }: Props): JSX.Element {
  return <H1>Hello, {name}!</H1>;
}
```

```typescript
// src/components/NameInput.tsx

import * as React from 'react';

import styled from 'styled-components';

export type Props = {
  name: string;
};

const Input = styled.input`
  border-radius: 4px;
  background-color: #fafafa;
  color: black;
  height: 16px;
  line-height: 16px;
`;

export default function NameInput({ name }: Props): JSX.Element {
  return <Input type="text" value={name} />;
}
```

```typescript
// src/components/Main.tsx

import * as React from 'react';

import { ReactSource } from '@cycle/react';
import { MemoryStream, Stream } from 'xstream';

import Greeter from '../Greeter';
import NameInput from '../NameInput';
import Incorporator from '../Incorporator';

export type SoReact = { react: ReactSource };
export type SiReact = { react: Stream<React.ReactElement<any>> };

export type Sources = { react: ReactSource };
export type Sinks = { react: Stream<React.ReactElement<any>> };

function Main({ react }: Sources): Sinks {
  const name$: MemoryStream<string> = react
    .select('event-input-name')
    .events('change')
    .map((e: any) => (e.persist(), e.target.value)) // ここで飛んでくる event は React の SyntheticEvent なので，module.hot があるときだけ persist したいがめんどくさい……
    .startWith('');

  const vdom$: Stream<React.ReactElement<any>> = name$.map((name) => (
    <div>
      <Greeter name={name} />
      <Incorporator sel="event-input-name">
        <NameInput name={name} />
      </Incorporator>
    </div>
  ));

  return {
    react: vdom$,
  };
}

export default Main;
```

```typescript
// src/index.ts

import { setup } from '@cycle/run';
import { makeDOMDriver } from '@cycle/react-dom';

const { rerunner, restartable } = require('cycle-restart');

import Main from './components/Main';

function makeDrivers() {
  return {
    react: restartable(makeDOMDriver(document.getElementById('app')), { pauseSinksWhileReplaying: false }),
  };
}

const rerun = rerunner(setup, makeDrivers);
rerun(Main);

if ((module as any).hot) {
  (module as any).hot.accept('./components/Main', () => {
    const { default: newMain } = require('./components/Main');

    rerun(newMain);
  });
}
```

`cycle-restart` で HMR もできる．試しにやってみるとこんな感じ．

![HMR](https://i.gyazo.com/f9711fd5b955ccf273428a1a777410b6.gif)

Web を書くときに Snabbdom の代わりに React を強く使いたい場面があるかというとちょっとわからないけれども，React Native が書けるというのは面白そう．

コード全文はこちらから．

[](https://github.com/ryota-ka/cycle-react-example)

## 脚注

[^1]: 既に React コミュニティに広く受け入れられているプラクティスも多々あるように見受けられる．
