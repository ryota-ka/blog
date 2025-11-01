---
keywords:
  - Cycle.js
  - React
---

# @cycle/reactを触ってみる

数日前，[@staltz](https://twitter.com/staltz)氏からCycle Reactのリリースがアナウンスされた．

https://twitter.com/andrestaltz/status/1019546640582152192

[`@cycle/react`](https://www.npmjs.com/package/@cycle/react)を含む一連のライブラリ群は，React componentをCycle.jsの中で用いることを可能にし，Cycle.jsのcomponentをReactの中で用いることを可能にする．

---

## モチベーション

氏は[Use React in Cycle.js and vice-versa](https://staltz.com/use-react-in-cyclejs-and-vice-versa.html)という記事で，次のように述べている．

> I have been critical of React, but that’s a normal thing as a framework author: you have different opinions on how frontend should be done. I’ve always been very positive about React Native, though, because it’s a game changer. It allows us to bring the JavaScript ecosystem to mobile development, without compromising (that much) on user experience.

[@hiroqn](https://github.com/hiroqn)に聞いた話だが，Cycle.jsでReact Nativeを書くというアイディアは数年前から既にあったらしく，[かつてのREADME](https://github.com/cyclejs/react-native/blob/cb5e1916a90142002040c13970fa25f58307961f/README.md)を読んでみると，CycleConf 2016のハッカソンで開発されたものが原型らしい．

氏はCycle Reactの存在には以下のような意義があるとしている．

- Reactユーザにとっては
  - 開発にCycle.jsのアーキテクチャを取り入れることができる
    - `cycle-onionify`のようなstate managementツールが利用可能になる
    - 様々なdriverを使って副作用を管理できるようになる
  - `@cycle/react`によって
    - コンポーネントを
      - `this`-lessに書ける
      - `class`を使わずに書ける
      - コード量が少なくなる
      - `const`だけで（immutableに）書ける
    - streamを使ったreactiveなプログラミングができる
- Cycle.jsユーザにとっては
  - Reactコミュニティに存在する数多の資産が利用可能になる
  - Cycle.jsで記述されたコンポーネントをReactユーザに対しても提供できる
  - React NativeをCycle.jsで記述できる

と，Reactユーザにとってはあまり嬉しい訳ではないような気がする[^1]が，Cycle.jsの側からすると，Reactコミュニティの資産にアクセスできるようになるのは魅力的である．

## 使い方

通常のCycle.jsアプリケーションとは異なり，`@cycle/dom`の代わりに`@cycle/react`を用いる．加えて，プラットフォームに合わせて`@cycle/react-dom`と`@cycle/react-native`のどちらかを選ぶ．

上記記事中のサンプルコードを見ればわかるが，基本的にはJSXは用いずに，`@cycle/react`がexportしている`h`関数を用いることを想定している．

Reactは`handleClick`といった書き方で，イベントが発生した際に関数をproactiveに呼び出す一方，Cycle.jsはreactiveなので，DOM側はどういったイベントを引き起こしうるのかは知らず，componentの中で`sources.DOM.select('.selector').events('click')`といった形でイベントのstreamを取得するという方式を取っている．しかしながら，CSSセレクタという概念が存在しないReact Nativeにも対応するためには，別の仕組みを用意するしかないので`@cycle/dom`とは少し事情が異なり，higher-order componentで囲むことによってこれを実現するというアプローチを取ったようだ．`sel`を渡すためにJSXは使えないので，hyperscriptを前提としたインタフェースを採用したらしい．実際記事中にも，`sel`が利用できないことを除けばJSXでも動作するし，将来的にJSXの完全なサポートがなされるかもしれないが，あくまでhyperscriptの方が優先，という趣旨の記述がある．現時点でJSXだけで済ませたい場合には，以下のようなSFCを用意しておくとなんとかなるが，あまり推奨されるようなものではないだろう．

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

ここからは実際に`@cycle/react`を用いたアプリケーションを作ってみる．冒頭で述べたとおり，`@cycle/react`を使えば

- React componentをCycle.js中で利用できる
- Cycle.jsのcomponentをReact中で利用できる

という2つの恩恵を得ることができるのだが，今回は前者のみを検証するCycle.jsアプリケーションを作成する．また，`@cycle/react-dom`と`@cycle/react-native`という選択肢があるが，今回は`@cycle/react-dom`を用いる．加えて，せっかくReactのエコシステムに乗っかるので，何かReactらしいものをと考えた結果，[`styled-components`](https://www.styled-components.com/)を取り入れることにした．

以下のパッケージをインストールする．

```console
$ yarn add @cycle/react @cycle/react-dom @cycle/run cycle-restart react react-dom styled-components xstream
$ yarn add -D @types/react @types/react-dom html-webpack-plugin ts-loader typescript webpack webpack-serve
```

コード（雑）はこんな感じ．

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
    .map((e: any) => (e.persist(), e.target.value)) // ここで飛んでくるeventはReactのSyntheticEventなので，module.hotがあるときだけpersistしたいがめんどくさい……
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

`cycle-restart`でHMRもできる．試しにやってみるとこんな感じ．

![HMR](https://i.gyazo.com/f9711fd5b955ccf273428a1a777410b6.gif)

Webを書くときにSnabbdomの代わりにReactを強く使いたい場面があるかというとちょっとわからないけれども，React Nativeが書けるというのは面白そう．

コード全文はこちらから．

https://github.com/ryota-ka/cycle-react-example

## 脚注

[^1]: 既にReactコミュニティに広く受け入れられているプラクティスも多々あるように見受けられる．
