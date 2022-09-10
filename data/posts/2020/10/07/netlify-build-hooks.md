---
keywords:
  - Netlify
---

# Netlify でホスティングしている Gatsby 製静的サイトを定期的にビルドして情報を更新する

## TL; DR

- Netlify の build hook URL を発行する
- 上記の URL に対して定期的にリクエストが送られるように設定する

---

## モチベーション

[ポートフォリオサイト](https://ryota-ka.me/)を [Gatsby](https://gatsbyjs.com/) で作って [Netlify](https://www.netlify.com/) でホスティングしている．そのコンテンツの一部として，[`gatsby-source-graphql`](https://www.gatsbyjs.com/plugins/gatsby-source-graphql/) を使って [GitHub GraphQL API](https://docs.github.com/en/free-pro-team@latest/graphql) から自分の profile の pinned items を取得して表示したり，[`gatsby-source-rss-feed`](https://www.gatsbyjs.com/plugins/gatsby-source-rss-feed/) を使ってこのブログの最新記事を取得して表示したりしている．静的サイトとしてホスティングしているため，これらの情報はビルド時に取得され，その時点で得られた値が HTML に埋め込まれる．しかし，こうした情報は低頻度ではあるものの，ポートフォリオサイトとは無関係に更新されるので，最新の情報を定期的に反映するため，一定時間ごとに再ビルドを行いたい．

## Build hooks

調べてみたところ，Netlify は [build hooks](https://docs.netlify.com/configure-builds/build-hooks/) という機能を提供していることがわかった．これは，Netlify 上で発行した，site に紐付く URL に対し，HTTP リクエストを送信することで，当該 site のビルドおよびデプロイをトリガするものである．

```sh
$ curl -X POST -d '{}' https://api.netlify.com/build_hooks/XXXXXXXXXXXXXXX
```

**Settings > Build & deploy** 内に **Build hooks** というセクションがあるので，その中の **Add build hook** ボタンを押せば build hook を追加できる．

![](https://gyazo.com/083a4d3358e3712115244890b1823aae.png)

「build hook URL に対して HTTP リクエストを送信する」という操作が定期的に実行されるように設定すれば，やりたかったことは実現できそうだ．

## GitHub Actions

定期実行といえば伝統的には crontab だが，今回は手軽な選択肢として GitHub Actions を使うことにする．

まずはリポジトリの secrets に build hook の URL を設定しておく．これは **Settings > Secrets > New secret** から行える．名前はなんでも構わないが，ここでは `NETLIFY_BUILD_HOOK_URL` にしておく．

![](https://gyazo.com/d548ba8a8d604e7cb019fd65802472f4.png)

次に，default branch で以下の YAML が `.github/workflows/` ディレクトリ内にある状態にする．今回は毎日0時 (日本時間) にビルドがトリガされるような設定にしている．

```yaml filename=.github/workflows/daily.yml
name: Daily build
on:
  schedule:
    - cron: '0 15 * * *' # UTC であることに注意
env:
  URL: ${{ secrets.NETLIFY_BUILD_HOOK_URL }}
jobs:
  build-hook:
    runs-on: ubuntu-latest
    steps:
      - name: Hook build
        run: |
          curl -X POST -d '{}' $URL
```

指定した時間が来れば，Netlify 上でビルドが開始されるだろう．

## 所感

各種クラウドサービスの発展により，個人でサーヴァを借りる時代でもなくなってしまった[^1]ので，Netlify のようなホスティングサービスはもとより，crontab の代替になるようなものが手軽に使えることは非常にありがたい．このような構成がリポジトリに置かれるべきかどうかについては議論の余地があるように思うが，少なくとも管理は随分と楽である．

## 脚注

[^1]: 6-7年ほど契約していた VPS も今年ついに解約した．
