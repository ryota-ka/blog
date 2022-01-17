# dotfiles を Nix + Home Manager に移行した

「ゴミの収集が止まる年末に大掃除をするのは非合理だ」という言説を見かけた．「一理ある」と感じたので，特に家の大掃除などはしないことに決め，代わりに dotfiles の大掃除をすることにした．プロたるもの，日頃から[^1]自らの仕事環境に対する投資を惜しんではならない．

筆者は Nix ユーザであるため，この年末年始の休暇を用いて Home Manager を導入したいと考えており，実際に移行を行った．

[](https://github.com/ryota-ka/dotfiles/commit/f5dfe8e72f46ee50620f518be4ca6ed1c262982a)

この記事では，Home Manager の利点や導入手順，実際の導入にあたって取った選択や tips を紹介する．

なお，筆者が用いているのは Intel および ARM プロセッサの macOS Monterey で，Zsh, Neovim, tmux などの上で生活している．もちろん Nix は GNU Linux にも対応しているので，仮に今後 Linux マシンをセットアップする機会があっても，滞りなく開発環境を構築できることが期待される．

---

## きっかけ

`ryota-ka/dotfiles` リポジトリ内にはこれまでも，各種の依存を Homebrew でインストールしたり，必要なシンボリックリンクを張ったりするインストールスクリプトとしての `Makefile` を用意していた．しかし今年の初頭に，マシンにインストールする開発用ソフトウェアはすべて Homebrew から Nix (`nix-env`) に移行しており，実際の作業環境との間で大きな乖離が生じていた．

昨今，開発用端末として Intel Mac から ARM Mac への移行を見据える必要性が生じ，開発検証用端末として社用の MacBook が手元に届いた．しかし，上記のような状況のため，快適とは程遠い環境での検証を強いられていた．そのため，この機会にセットアップが簡易な dotfiles を用意することにした．

余談だが，手元の Homebrew の環境は完全に壊れてしまっている．もう使っていないので特に問題はないのだが，経験上 Ruby の環境はよく壊れるのであまり関わりたくない[^2]．

```sh
$ brew doctor
/usr/local/Homebrew/Library/Homebrew/standalone/sorbet.rb:11:in `require': cannot load such file -- sorbet-runtime-stub (LoadError)
        from /usr/local/Homebrew/Library/Homebrew/standalone/sorbet.rb:11:in `<top (required)>'
        from /usr/local/Homebrew/Library/Homebrew/startup/sorbet.rb:9:in `require'
        from /usr/local/Homebrew/Library/Homebrew/startup/sorbet.rb:9:in `<top (required)>'
        from /usr/local/Homebrew/Library/Homebrew/startup.rb:10:in `require_relative'
        from /usr/local/Homebrew/Library/Homebrew/startup.rb:10:in `<top (required)>'
        from /usr/local/Homebrew/Library/Homebrew/global.rb:4:in `require_relative'
        from /usr/local/Homebrew/Library/Homebrew/global.rb:4:in `<top (required)>'
        from /usr/local/Homebrew/Library/Homebrew/brew.rb:29:in `require_relative'
        from /usr/local/Homebrew/Library/Homebrew/brew.rb:29:in `<main>'
```

## Nix について

えっ，まだ Nix を使っていない！？HERPでは必修ですよ！！[^3]

## Home Manager について

[](https://github.com/nix-community/home-manager)

Home Manager は，俗に "dotfiles" と呼ばれる各種ソフトウェアの設定ファイルや，ユーザ環境にインストールされるべきパケッジを宣言的に記述するためのツールである．設定の記述は Nix expression language を用いて行う．例えば，以下のような設定ファイルを記述した上で `$ home-manager switch` を実行すると，Nix でインストールされた Git が利用可能になり，生成された Git の設定ファイルへのシンボリックリンクが `~/.config/git/config` に作成される．

```nix
{ pkgs, ... }:

{
  home.username = "ryota-ka";
  home.homeDirectory = "/Users/ryota-ka";
  home.stateVersion = "22.05";

  programs.git = {
    enable = true;

    userName = "Ryota Kameoka";
    userEmail = "ok@ryota-ka.me";
    aliases = {
      br = "branch";
      co = "checkout";
    };
  };
}
```

[インストール手順は公式マニュアルに譲る](https://nix-community.github.io/home-manager/index.html#sec-install-standalone)が，手元の環境では最近 `$NIX_PATH` がセットされないという現象が生じており，インストールに当たって以下の環境変数を設定した．

```sh
$ export NIX_PATH=nixpkgs=~/.nix-defexpr/channels/nixpkgs:home-manager=~/.nix-defexpr/channels/home-manager
```

また，インストール後，`$ home-manager switch` を実行する段階で，既にインストールされているパケッジと競合する旨のエラーが発生した．優先度が同じだと競合するようなので，以下のコマンドを実行して優先度を調整した．

```sh
$ nix-env --set-flag priority 10 nix
$ nix-env --set-flag priority 10 home-manager-path
```

## これまでの `ryota-ka/dotfiles`

移行前の `ryota-ka/dotfiles` リポジトリは概ね以下のようなディレクトリ構造を持っていた．

```plaintext
.
├── .gitconfig
├── .gitignore
├── .gitignore_global
├── .tmux.conf
├── .vim/
│   ├── (ftdetect/ や ftplugin/ など)
│   └── plugin/
│       └── (様々な設定ファイルたち)
├── .vimrc (.vim/ を runtimepath に追加し，VimPlug でインストールされるプラグインを記述する)
├── .zsh/
│   ├── .zlogin
│   ├── .zshrc
│   ├── functions/
│   │   └── (独自に定義した便利関数たち)
│   ├── functions.zsh
│   └── (その他主に .zlogin から読まれる様々な設定ファイルたち)
├── .zshenv
├── Makefile
└── karabiner.json
```

基本的にはこれらのファイルへのシンボリックリンクをホームディレクトリに作成するという戦略を取っていた．例えば，`~/.gitconfig` は `/path/to/the/repo/.gitconfig` を指すリンク，`~/.vim` は `/path/to/the/repo/.vim/` を指すリンク，といった具合である．

また，`~/.zsh` に `.zsh/` ディレクトリへのシンボリックリンクを作成し，`$ZDOTDIR` として `~/.zsh` を指定する設定を `.zshenv` 内に記述していた．

`.vimrc` へのシンボリックリンクは `~/.config/nvim/init.vim` にも作成しており，Neovim からも利用可能にしていた．`.vim/` 以下のファイルは，`.vimrc` に `set runtimepath+=~/.vim` と記述することで参照されるようになっていた．

## 方針・設定など

Home Manager はデフォルトでは `~/.config/nixpkgs/home.nix` という設定ファイルを参照しようとするが，`-f` オプションを渡すと別のファイルを指定することができる．そこで，リポジトリ直下に `home.nix` というファイルを作成することにした．設定を変更した際には，`$ home-manager -f ./home.nix switch` を実行することで，記述した設定内容がユーザ環境に反映される．以下では `-f` オプションは省略する．

知らぬ間に世間では [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html) が普及しており，Home Manager もこれに準拠している．例えば Git の設定ファイルは `~/.gitconfig` ではなく (`$XDG_CONFIG_HOME` を特に設定していない場合) `~/.config/git/config` に置かれることになる．

大学生の頃から8年近くに渡って継ぎ足してきた dotfiles なので，とりわけ記述量の多い Zsh や Neovim の設定ファイルに関しては，すべてを Nix expression language の側に寄せるのは初めの一歩としては歩幅が大きすぎる．そこで，これらに関しては既存の `.zsh` ファイルや `.vim` ファイルをうまく活用する形で移行の実現を試みた．

### Git

`.gitconfig` に書いていた設定項目はさほど量が多くないため，すべての設定を素直に移行することができた．特に工夫した点もないので解説の必要もないが，以下の点は特筆すべきである．

筆者は `git-diff(1)` の表示に `diff-highlight` を用いている．

[](https://github.com/git/git/tree/master/contrib/diff-highlight)

この executable がインストールされるパスが，Homebrew で Git をインストールする場合と Nix で Git をインストールする場合とで異なるのだが，いちいちアドホックに `$PATH` を通すのも馬鹿馬鹿しい．しかし，Nix expression language で設定を記述すれば，Git がインストールされるディレクトリ名が事前に計算できるため，信頼性の高い絶対パスを埋め込むことができる．

```nix
programs.git = {
  enable = true;

  extraConfig = {
    interactive = {
      diffFilter = "${pkgs.git}/share/git/contrib/diff-highlight/diff-highlight";
    };
  };
};
```

このような設定から以下のような記述が生成される．

```ini name=.gitconfig
[interactive]
	diffFilter = "/nix/store/mf88kd4884mc47bk43ayp75x97km8hvf-git-2.33.1/share/git/contrib/diff-highlight/diff-highlight"
```

### Zsh

Zsh の設定のコア部分を抜き出したものが以下である．

```nix
programs.zsh = {
  enable = true;

  defaultKeymap = "emacs";

  # ファイルが生成されるディレクトリ ($ZDOTDIR)
  dotDir = ".config/zsh";

  # zsh-syntax-highlighting を有効化する
  enableSyntaxHighlighting = true;

  # .zshenv に追記される内容
  envExtra = ''
    if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then
      . ~/.nix-profile/etc/profile.d/nix.sh
    fi
  '';

  # .zlogin に追記される内容
  loginExtra = ''
    FPATH=${./.zsh/functions}:$FPATH

    . ${./.zsh/foo.zsh}
    . ${./.zsh/bar.zsh}
    . ${./.zsh/baz.zsh}
  '';
};
```

この状態で `$ home-manager switch` を実行すると，Nix で Zsh がインストールされ，`~/.config/zsh/` 以下に `.zshenv` `.zshrc` `.zlogin` などのファイルが作成される．

作成された`~/.config/zsh/.zshrc` は概ね以下のようになっている．ただしコメントや空行は適宜省略した．

```sh name=~/.config/zsh/.zshrc
typeset -U path cdpath fpath manpath

for profile in ${(z)NIX_PROFILES}; do
  fpath+=($profile/share/zsh/site-functions $profile/share/zsh/$ZSH_VERSION/functions $profile/share/zsh/vendor-completions)
done

HELPDIR="/nix/store/d1psg5mf2shj8s674inyx816vbzdc3n5-zsh-5.8/share/zsh/$ZSH_VERSION/help"

bindkey -e

autoload -U compinit && compinit

source /nix/store/vl4h8nj3w0b1km44bkz24dad5gs8sjrf-zsh-syntax-highlighting-0.7.1/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

. "/Users/ryota-ka/.nix-profile/etc/profile.d/hm-session-vars.sh"
```

また `~/.config/zsh/.zlogin` は以下のような内容になっている．

```sh name=~/.config/zsh/.zlogin
FPATH=/nix/store/js2lfqqnb72bqsz38w6dc2qgg7zz0spi-functions:$FPATH

. /nix/store/8bj838sz917w3g58pm9bbfnp68hsdl21-foo.zsh
. /nix/store/xcbgzqlycb4c0bnmx1kg3iws9jcczxc5-bar.zsh
. /nix/store/8wfmnmpqjwwl52j6apgpvpb0y8ckpq7z-baz.zsh
```

`loginExtra` に渡す string に path 型の値を埋め込むことで，当該ファイル (e.g. `.zsh/foo.zsh`) が `/nix/store/` ディレクトリ以下に入っていることがわかる．`.zsh/foo.zsh` や `.zsh/functions/*.zsh` はもはや `$ZDOTDIR` として指定するディレクトリ (`~/.config/zsh/`) 以下には存在しないが，ファイルの位置が適切に解決されるなるならば，もとより入れておく必要もなかったことに気付く．このようにして，既存の `.zsh` ファイルと Home Manager とのブリッジングを行った．

### Neovim

筆者は [VimPlug](https://github.com/junegunn/vim-plug) を用いて各種プラグインを管理している．Nix のエコシステムでは `pkgs.vimPlugins.*` で種々の Vim プラグインの derivation が利用可能であり，Home Manager も `programs.neovim.plugins` にプラグインのリストを渡すことで，Neovim のパケッジ・マネジャー機構を用いて統合を行ってくれる．しかし，順調に乗り換えられるか否かという不確実性を嫌い，今回は引き続き VimPlug を用いてプラグインを管理することにした．

VimPlug の標準的なインストール方法は，cURL を用いて所定のパスにファイルを配備するというものであるため，Nix との相性が悪い．そこで，VimPlug だけは Nix 経由で調達することにした．`pkgs.vimPlugins.vim-plug` という derivation が存在するので，これをビルドした結果のディレクトリ内に存在する `plug.vim` というファイルを `init.vim` から読み込むことにする．

```nix
programs.neovim = {
  enable = true;

  extraConfig = ''
    " 従前の設定ファイルを置いているディレクトリを runtimepath に追加
    set runtimepath+=${./.vim}

    " VimPlug をロード
    source ${pkgs.vimPlugins.vim-plug}/plug.vim

    " VimPlug で管理するプラグインの一覧 (かつての .vimrc) をロード
    source ${./plugins.vim}
  '';
  withNodeJs = true;
};
```

ここで `withNodeJs = true;` に注目したい．これは Node.js provider を有効化するオプションである．仮に自前で Node.js provider をセットアップしようとすれば，何らかの手法で Node.js をインストールし，`$ npm install -g neovim` を実行してライブラリをインストールする必要がある．また，Python provider をセットアップしようとすれば，何らかの手法で Python 3 をインストールし，`$ pip install neovim` でライブラリをインストールしなければならない．各言語固有のパケッジ・マネジャーを用いてライブラリをインストールしていくと環境がとっ散らかりがちだが，`withNodeJs` オプションや `withPython3` オプション (デフォルトで有効) を指定するだけで Nix 側でインストールの面倒を見てくれ，必要なものを勝手に調達してくれるのはなかなか体験がいい．また，Nix に管理を任せることで，不要になった際にはガーベジ・コレクションさえ行ってくれる．

さて，上記の設定を元に `$ home-manager switch` を実行すると，以下のような `~/.config/nvim/init.vim` が生成される．

```vim
set packpath^=/nix/store/617lyrfdxhg5dy69yvmb6h9rv6xmar3f-vim-pack-dir
set runtimepath^=/nix/store/617lyrfdxhg5dy69yvmb6h9rv6xmar3f-vim-pack-dir

" 従前の設定ファイルを置いているディレクトリを runtimepath に追加
set runtimepath+=/nix/store/6yzyhflsjbldp6rzrlj8gskkdn3g2559-.vim

" VimPlug をロード
source /nix/store/65qsrxp0q8xphnlqbyv9ip5wzvdvczgh-vimplugin-vim-plug-2021-08-31/plug.vim

" VimPlug で管理するプラグインの一覧 (かつての .vimrc) をロード
source /nix/store/z8764xksvr6n9cxcwj3ra7rs8ibbv5h2-plugins.vim
```

Zsh のときと同じく，参照したいファイルないしディレクトリへのパスを埋め込んだ文字列を元に，Home Manager に設定ファイルを作らせる事によって，当該ファイルやディレクトリを Nix の管理下に置き，既存の `.vim` ファイルとのブリッジングを行っている．

### その他のソフトウェア

[direnv](https://github.com/direnv/direnv) なども Home Manager に統合されている．direnv を使うには，インストールした上で，普段使っているシェルの設定ファイルに自分で hook を一行書き加える必要があるが，Home Manager を用いるとそのような設定を `.zshrc` に自動的に差し込んでくれる．これは「ユーザ環境の設定」という行為を一箇所で包括的に行うからこそできる芸当だろう．

## 読むべき資料

以下のページに，設定可能なオプションが網羅的に列挙されている．

[](https://rycee.gitlab.io/home-manager/options.html)

しかし，網羅的であるがゆえに自分の興味の範疇を出たものも多いであろう．そこで，GitHub 上で Home Manager の実装を参照し，興味のあるソフトウェアの module を個別に眺めていくことをおすすめする．例えば以下は `programs.neovim` に対応する module である．

[](https://github.com/nix-community/home-manager/blob/master/modules/programs/zsh.nix)

また，筆者が実際に行った設定を眺めるのも参考になるだろう．

[](https://github.com/ryota-ka/dotfiles)

## 脚注

[^1]: 年末に大掃除をするのではなく普段から掃除をしましょう．
[^2]: 個人の感想です．
[^3]: 社内の開発における Nix 活用に関しては別の機会に記事にしたいと思っている．
