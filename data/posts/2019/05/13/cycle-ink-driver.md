# cycle-ink-driver をリリースした

`cycle-ink-driver` というライブラリをリリースした．このライブラリを用いると，[Cycle.js](https://cycle.js.org) と [Ink](https://github.com/vadimdemedes/ink) を用いてインタラクティヴなコマンドライン・アプリケーションを作ることができる．

[](https://www.npmjs.com/package/cycle-ink-driver)

[](https://github.com/ryota-ka/cycle-ink-driver)

---

Cycle.js は Web フロントエンドフレームワークではない．Cycle.js は Web フロントエンドフレームワークではない．大事なので2回言いました．

[Cycle.js](https://cycle.js.org) を，例えば [React](https://reactjs.org) などと比較するのは間違っている，と再来週の [effect system 勉強会](https://connpass.com/event/124786/) で話そうと思っている．Cycle.js は effect system の枠組みを提供しているに過ぎず，view の部分は ReactDOM の代わりに [Snabbdom](https://github.com/snabbdom/snabbdom)，state 管理の部分は [Redux](https://redux.js.org) の代わりに [Cycle State](https://cycle.js.org/api/state.html) が提供されており，component はこうした effect を好きに並べ立て，実行時に driver なり higher-order component なりで解決する，というのが最近の個人的な解釈である[^1]．

実際，Cycle.js を使いながらも，view 部分に Snabbdom ではなく React を用いることができる．component が Snabbdom に関する effect ではなく React に関する effect を要求し，これを解決できればそれで済む話なのだ．これを実現する `@cycle/react` に関しては昨年にエントリを書いているので参照されたい．

[](https://ryota-ka.hatenablog.com/entry/2018/07/22/180141)

さて，Ink は，React の仕組みを用いて，インタラクティヴなコマンドライン・アプリケーションを作るためのライブラリである．前述の通り，Cycle.js と React との統合は既になされているので，Web フロントエンド・アプリケーションを書きたい場合には，ReactDOM と [Cycle ReactDOM](https://www.npmjs.com/package/@cycle/react-dom) を使えばよいのと同じく，CLI アプリケーションを書きたい場合には，Ink と `cycle-ink-driver` を使えばよい．

| レンダラ | アダプタ           | プラットフォーム |
| -------- | ------------------ | ---------------- |
| ReactDOM | Cycle ReactDOM     | Web              |
| Ink      | `cycle-ink-driver` | CLI              |

試しに，Cycle.js オフィシャルサイトのトップページにあるデモのクローンを作ってみた．

![](https://i.gyazo.com/d479ca6009d4e1c0e263cd41de6cfb09.gif)

```typescript
/** @jsx createIncorporatedElement */

import { ReactSource } from '@cycle/react';
import { run } from '@cycle/run';
import { createIncorporatedElement, makeInkDriver } from 'cycle-ink-driver';
import { Box } from 'ink';
import TextInput from 'ink-text-input';
import { Stream } from 'xstream';

const sels = {
  name: Symbol('name'),
};

function View({ name }: { name: string }): JSX.Element {
  return (
    <Box flexDirection="column">
      <Box>
        Name: <TextInput sel={sels.name} value={name} onChange={() => {}} />
      </Box>
      {'-'.repeat(20)}
      <Box>Hello {name}</Box>
    </Box>
  );
}

function main({ react }: { react: ReactSource }): { react: Stream<JSX.Element> } {
  const name$ = react.select(sels.name).events('change').startWith('');

  const view$ = name$.map((name) => View({ name }));

  return {
    react: view$,
  };
}

const drivers = {
  react: makeInkDriver(),
};

run(main, drivers);
```

コード全文はこちらから．

[](https://github.com/ryota-ka/cycle-ink-example)

## 謝辞

実装にあたって，[`sliptype/cycle-react-pragma`](https://github.com/sliptype/cycle-react-pragma) を参考にした．Cycle React 本体に merge されることを願っている．

## 脚注

[^1]: このあたりの気持ちについては別途エントリを書きたいと考えている．
