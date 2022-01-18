# Dhall で Kubernetes の YAML 管理をスマートにやっていく

こんにちは．Kubernetes 班の ryota-ka です。皆さん Kubernetes やっていますか？[^1]

Kubernetes をやっていこうとすると，大量の YAML を書くことになって大変である．大量の YAML を書くことは大変なので，大抵コピペする．コピペをするが，コピペは怖い．例えば，deployment を定義する YAML をコピペしたとして，万一 label を変更するのを忘れて，想定していない service からルーティングされたりすると悲惨である．そもそも(個人的な意見ではあるが) YAML の仕様自体があまりにも複雑かつ難解であり[^2]，ある種の仕様が余計なお世話だともしばしば感ぜられる[^3]．

最近では [kustomize](https://github.com/kubernetes-sigs/kustomize) が将来的に kubectl に統合される予定で開発が進められているが，この記事では異なる切り口として，Dhall lang を用いてプログラマティックに YAML ファイルを生成するという方法をご紹介したい．

---

## Dhall について

[Dhall](https://github.com/dhall-lang/dhall-lang) は，かの Gabriel Gonzalez 氏が中心になって開発している設定記述言語である．[README によると](https://github.com/dhall-lang/dhall-lang#name) "tall" / "call" / "hall" と韻を踏むことから，アメリカ英語では /dɔl/，イギリス英語では /dɔːl/ と発音するそうだ．カタカナを当てはめてみるとすると「ドール」だろうか．

[GitHub の dhall-lang/dhall-lang](https://github.com/dhall-lang/dhall-lang) では以下のような説明がなされている．

> A configuration language guaranteed to terminate
>
> Dhall is a programmable configuration language that is not Turing-complete
> You can think of Dhall as: JSON + functions + types + imports

Dhall の特徴的な性質として，**チューリング完全でない**ことが挙げられる．Dhall は設定ファイルを記述するための言語であり，汎用プログラミング言語ではないので，高い計算能力を必要としないのだ．雑に言ってしまうと，再帰や無限ループを書けない．チューリング完全でないことの嬉しさとして，評価が停止することが保証できる．雑に言ってしまうと，再帰や無限ループが書けないのでちゃんと停止する．理論が好きな方への説明も申し添えておくと，Dhall におけるすべての式は一意な正規形を持つ．

Nix を使っていれば，以下のコマンドでインストールできる．

```sh
$ nix-env -iA nixpkgs.dhall nixpkgs.dhall-json
```

環境を汚したくなければもちろん `nix-shell` を利用してもよい．

```sh
$ nix-shell -p dhall dhall-json
```

executable として `dhall` `dhall-to-json` `dhall-to-yaml` が利用できることを確認してほしい．

`dhall repl` で REPL が起動する．

```sh
$ dhall repl
⊢ 1 + 1

2
```

ghci と同じく，`:type` または `:t` で，与えられた式の型を表示する．

```dhall
⊢ :t 42

Natural
```

## Dhall expression 入門

あまり詳細に解説すると，それだけで一つの記事になってしまうので，この記事中で用いるものの説明のみで済ませる．より詳しく知りたい方は [GitHub Wiki](https://github.com/dhall-lang/dhall-lang/wiki) を参照のこと．基礎的な文法について詳しく解説することはしないが，このブログの読者であれば，Haskell ないし OCaml の基本的な文法を心得ていると思うので，特に問題ないだろう[^4]．

### `Bool` 型

リテラル `False` および `True` は `Bool` 型をもつ．

```shall
⊢ :t False

Bool
```

### `Natural` 型

符号なしの数値リテラルは `Natural` 型をもつ．

```dhall
⊢ :t 42

Natural
```

ちなみに符号を付ければ `Integer` 型．このあたりの表記は最近破壊的変更があった．

### `Text` 型

ダブルクォートで囲む文字列リテラルは `Text` 型をもつ．

```shall
⊢ :t "Dhall"

Text
```

Nix 形式の複数行文字列リテラルも提供されている．

```sh
$ dhall <<EOF
heredoc> ''
heredoc> Dhall is a programmable configuration language that is not Turing-complete
heredoc>
heredoc> You can think of Dhall as: JSON + functions + types + imports
heredoc> ''
heredoc> EOF
```

```dhall
Text

''
Dhall is a programmable configuration language that is not Turing-complete

You can think of Dhall as: JSON + functions + types + imports
''
```

`${...}` による interpolation もサポートされている．

```dhall
⊢ let name = "ryota-ka" in "Hello, ${name}!"

"Hello, ryota-ka!"
```

文字列の結合は `++` 演算子．

```dhall
⊢ "Hello, " ++ "world!"

"Hello, world!"
```

### `List` 型

ブラケットで囲むと `List` 型になる．

```dhall
⊢ :t [6, 28, 496]

List Natural
```

リストの結合は `#` 演算子．

```dhall
⊢ [0, 1, 2] # [3, 4]

[ 0, 1, 2, 3, 4 ]
```

### `Optional` 型

`Optional` 型のリテラルは，リストと同じでブラケットを用いる．リテラルを共有している都合上，型注釈が必須となっている．型注釈は `value : Type` という形式で書く．

```dhall
⊢ [42] : Optional Natural

[ 42 ] : Optional Natural


⊢ [] : Optional Natural

[] : Optional Natural
```

### 関数型

関数は以下のような構文で表される．引数の型注釈は必須．

```dhall
⊢ :t λ(n : Natural) → n * 2

∀(n : Natural) → Natural
```

適用はもちろんスペース．

```dhall
⊢ let double = λ(n : Natural) → n * 2 in double 42

84
```

### `Type` 型

`Bool` や `Natural`，`List` などの型も型をもつ．「型の型」は Haskell などでは「族」と呼ばれているものだが，Dhall ではこれらも立派に値の型として作用する．後で見るが，型を値をコンテクストで利用できるからである．

例えば，`Bool` は `Type` 型をもつ．

```dhall
⊢ :t Bool

Type
```

`Natural → Natural` も `Type` 型をもつ．

```dhall
⊢ :t Natural → Natural

Type
```

`List` は具体型を一つ受け取ると具体型になるので，`Type → Type` という型をもつ．

```dhall
⊢ :t List

Type → Type
```

「型を値をコンテクストで利用できる」ことを確かめる．以下の例では，値である変数 `x` が型注釈の後ろ，すなわち型として出現していることから，値を型として利用できることがわかる．

```dhall
⊢ let x = Bool in False : x

False
```

型を値として扱えるので，もちろん型を引数として取るような関数も定義できる．以下は `Some` とでも名付けられそうな関数の例である[^5]．

```dhall
⊢ :t λ(t : Type) → λ(x : t) → [x] : Optional t

∀(t : Type) → ∀(x : t) → Optional t
```

### レコード型

レコード型は0個以上の key-value の組である．

```dhall
⊢ :t { name = "ryota-ka", age = 25 }

{ name : Text, age : Natural }
```

空のレコードの値は，空のレコードの型 `{}` と区別するため，`{=}` と書く．

```dhall
⊢ :t {=}

{}
```

`⫽` 演算子を用いて，複数のレコードをマージすることができる．同じ key が存在する場合には，右側のものが優先される．

```dhall
⊢ { foo = 42 } ⫽ { bar = "Hello", baz = {=} } ⫽ { foo = "Yo" }

{ foo = "Yo", bar = "Hello", baz = {=} }
```

### union型

任意個の型の和を表す型は次のように記述する．

```dhall
⊢ <Left = 42 | Right : Text> : < Left : Natural | Right : Text>

< Left = 42 | Right : Text >

⊢ <Left : Natural | Right = "foo"> : < Left : Natural | Right : Text>

< Right = "foo" | Left : Natural >
```

慣例的に `Left` `Right` というタグを用いたが，任意の名前を使うことができる．

```dhall
⊢  < Top : Natural | Middle : Bool | Bottom : Text>

< Top : Natural | Middle : Bool | Bottom : Text >
```

少々冗長だが，`constructors` キーワードを利用することで記述性が改善されるが，今回は割愛する．

### ファイルインポート

他のファイルに書かれた Dhall expression を読み込みたい場合はどうすればよいだろうか．Dhall では，絶対パスや相対パスも expression として扱われ，パスが指し示すファイルの内容を Dhall expression として解釈した式として扱われる．

例えば，`cube.dhall` に以下の内容が記述されているとする．

```sh
$ cat cube.dhall
let cube = λ(n : Natural) → n * n * n in cube
```

この状態で，`./cube.dhall` は valid な Dhall expression であり，上述の関数が式の値となる．

```dhall
λ(n : Natural) → n * n * n

⊢ ./cube.dhall

λ(n : Natural) → n * n * n


⊢ ./cube.dhall 5

125
```

また，Dhall は distributed であることを謳っており，ファイルパスだけではなく，URL を用いた参照も行える．

```dhall
⊢ https://raw.githubusercontent.com/dhall-lang/Prelude/master/List/all

  λ(a : Type)
→ λ(f : a → Bool)
→ λ(xs : List a)
→ List/fold a xs Bool (λ(x : a) → λ(r : Bool) → f x && r) True


⊢ https://raw.githubusercontent.com/dhall-lang/Prelude/master/List/all Natural Natural/odd [3, 5, 7, 11, 13]

True
```

## Kubernetes の設定を記述する - deployment と service を例に

さて，ここからは実際に Dhall を用いて，Kubernetes の設定を記述する YAML ファイルを作っていこう．

まず，記述したいサービスを表現するデータ型を用意する．管理したいサービスは，基本的にすべてこのデータ型で記述することを目標としたい．今回は簡単なデータ型だが，必要に応じて拡張すればよい．

```dhall
-- OurService.dhall

{ containerPort : Natural
, environment   : List { name : Text, value : Text }
, image         : Text
, name          : Text
, replicas      : Natural
, servicePort   : Natural
, tag           : Text
}
```

また，この型をもつ値を別のファイルに定義する．今回は Nginx を動かすことを想定してみよう．

```dhall
-- nginx.dhall

    let OurService = ./OurService.dhall

in  let service
        : OurService
        = { containerPort = 80
          , environment   = [ { name = "NGINX_PORT", value = "80" } ]
          , image         = "nginx"
          , name          = "nginx"
          , replicas      = 1
          , servicePort   = 8080
          , tag           = "1.15.2"
          }

in  service
```

`OurService` から deployment を生成する式は以下のようになる．`Deployment` 型は[ここ](https://github.com/dhall-lang/dhall-kubernetes/blob/master/types/io.k8s.api.apps.v1.Deployment.dhall) で定義されている．

```dhall
-- deployment.yaml.dhall

    let Some =
          https://raw.githubusercontent.com/dhall-lang/Prelude/c79c2bc3c46f129cc5b6d594ce298a381bcae92c/Optional/Some

in  let Deployment =
          https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/types/io.k8s.api.apps.v1beta2.Deployment.dhall

in  let Spec =
          https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/types/io.k8s.api.apps.v1beta2.DeploymentSpec.dhall

in  let PodSpec =
          https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/types/io.k8s.api.core.v1.PodSpec.dhall

in  let ContainerPort =
          https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/types/io.k8s.api.core.v1.ContainerPort.dhall

in  let defaultDeployment =
          https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/default/io.k8s.api.apps.v1beta2.Deployment.dhall

in  let defaultMeta =
          https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/default/io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall

in  let defaultSpec =
          https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/default/io.k8s.api.apps.v1beta2.DeploymentSpec.dhall

in  let defaultTemplate =
          https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/default/io.k8s.api.core.v1.PodTemplateSpec.dhall

in  let defaultPodSpec =
          https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/default/io.k8s.api.core.v1.PodSpec.dhall

in  let defaultSelector =
          https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/default/io.k8s.apimachinery.pkg.apis.meta.v1.LabelSelector.dhall

in  let defaultContainer =
          https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/default/io.k8s.api.core.v1.Container.dhall

in  let defaultContainerPort =
          https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/default/io.k8s.api.core.v1.ContainerPort.dhall

in  let OurService = ./OurService.dhall

in    λ(svc : OurService)
    →     let selector =
                Some
                (List { mapKey : Text, mapValue : Text })
                [ { mapKey = "app", mapValue = svc.name } ]

      in  let spec =
                  defaultSpec
                  { selector = defaultSelector ⫽ { matchLabels = selector }
                  , template =
                        defaultTemplate
                        { metadata =
                              defaultMeta { name = svc.name }
                            ⫽ { labels = selector }
                        }
                      ⫽ { spec =
                            Some
                            PodSpec
                            ( defaultPodSpec
                              { containers =
                                  [   defaultContainer { name = svc.name }
                                    ⫽ { image           =
                                          Some
                                          Text
                                          (svc.image ++ ":" ++ svc.tag ++ "")
                                      , imagePullPolicy =
                                          Some Text "IfNotPresent"
                                      , ports           =
                                          Some
                                          (List ContainerPort)
                                          [ defaultContainerPort
                                            { containerPort = svc.containerPort
                                            }
                                          ]
                                      }
                                  ]
                              }
                            )
                        }
                  }
                ⫽ { replicas             = Some Natural svc.replicas
                  , revisionHistoryLimit = Some Natural 10
                  }

      in  (     defaultDeployment { metadata = defaultMeta { name = svc.name } }
              ⫽ { spec = Some Spec spec }
            : Deployment
          )
```

一方 service はこんな感じ．`Service` 型の定義は[ここ](https://github.com/dhall-lang/dhall-kubernetes/blob/master/types/io.k8s.api.core.v1.Service.dhall)．

```dhall
-- deployment.yaml.dhall

    let Some =
          https://raw.githubusercontent.com/dhall-lang/Prelude/c79c2bc3c46f129cc5b6d594ce298a381bcae92c/Optional/Some

in  let None =
          https://raw.githubusercontent.com/dhall-lang/Prelude/c79c2bc3c46f129cc5b6d594ce298a381bcae92c/Optional/None

in  let Service =
          https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/types/io.k8s.api.core.v1.Service.dhall

in  let ServiceSpec =
          https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/types/io.k8s.api.core.v1.ServiceSpec.dhall

in  let ServicePort =
          https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/types/io.k8s.api.core.v1.ServicePort.dhall

in  let ObjectMeta =
          https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/types/io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall

in  let defaultService =
          https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/default/io.k8s.api.core.v1.Service.dhall

in  let defaultMeta =
          https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/default/io.k8s.apimachinery.pkg.apis.meta.v1.ObjectMeta.dhall

in  let defaultServicePort =
          https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/default/io.k8s.api.core.v1.ServicePort.dhall

in  let defaultSpec =
          https://raw.githubusercontent.com/dhall-lang/dhall-kubernetes/master/default/io.k8s.api.core.v1.ServiceSpec.dhall

in  let OurService = ./OurService.dhall

in    λ(svc : OurService)
    →     let selector =
                Some
                (List { mapKey : Text, mapValue : Text })
                [ { mapKey = "app", mapValue = svc.name } ]

      in  let ports =
                Some
                (List ServicePort)
                [   defaultServicePort { port = svc.servicePort }
                  ⫽ { protocol   = Some Text "TCP"
                    , targetPort =
                        Some
                        < Int : Natural | String : Text >
                        < Int = svc.containerPort | String : Text >
                    }
                ]

      in  let metadata
              : ObjectMeta
              = defaultMeta { name = svc.name } ⫽ { labels = selector }

      in  let spec
              : ServiceSpec
              =   defaultSpec
                ⫽ { ports    = ports
                  , selector = selector
                  , type     = Some Text "LoadBalancer"
                  }

      in  (     defaultService { metadata = metadata }
              ⫽ { spec = Some ServiceSpec spec }
            : Service
          )
```

`deployment.yaml.dhall` は `∀(svc : OurService) → Deployment`，`service.yaml.dhall` は `∀(svc : OurService) → Service` なる関数である．`nginx.dhall` に定義されている `OurService` 型の値に適用することで，`Deployment` 型及び `Service` 型の値を得ることができる．`dhall-to-yaml` を用いて，これらの値を YAML に出力してみよう．`./deployment.yaml.dhall ./nginx.dhall` という式は関数適用であることに注意されたい．

```sh
dhall-to-yaml --omitNull <<< './deployment.yaml.dhall ./nginx.dhall' > ./dist/nginx.deployment.yaml
dhall-to-yaml --omitNull <<< './service.yaml.dhall ./nginx.dhall' > ./dist/nginx.service.yaml
```

以下のような YAML が得られる．

```yaml
# dist/nginx.deployment.yaml
apiVersion: apps/v1beta2
kind: Deployment
spec:
  revisionHistoryLimit: 10
  selector:
    matchLabels:
      app: nginx
  template:
    spec:
      containers:
        - image: nginx:1.15.2
          imagePullPolicy: IfNotPresent
          name: nginx
          ports:
            - containerPort: 80
    metadata:
      name: nginx
      labels:
        app: nginx
  replicas: 1
metadata:
  name: nginx
```

```yaml
# dist/nginx.service.yaml
apiVersion: v1
kind: Service
spec:
  selector:
    app: nginx
  ports:
    - targetPort: 80
      protocol: TCP
      port: 8080
  type: LoadBalancer
metadata:
  name: nginx
  labels:
    app: nginx
```

これらの YAML を Kubernetes に食わせると，Nginx が動く．私は Docker for Mac の Kubernetes で試した．

```sh
$ kubectl apply -f ./nginx.deployment.yaml -f ./nginx.service.yaml
$ curl localhost:8080
<!DOCTYPE html>
<html>
<head>
<title>Welcome to nginx!</title>
<style>
    body {
        width: 35em;
        margin: 0 auto;
        font-family: Tahoma, Verdana, Arial, sans-serif;
    }
</style>
</head>
<body>
<h1>Welcome to nginx!</h1>
<p>If you see this page, the nginx web server is successfully installed and
working. Further configuration is required.</p>

<p>For online documentation and support please refer to
<a href="http://nginx.org/">nginx.org</a>.<br/>
Commercial support is available at
<a href="http://nginx.com/">nginx.com</a>.</p>

<p><em>Thank you for using nginx.</em></p>
</body>
</html>
```

これだけの例ではあまり恩恵を感じることができないかもしれないが，管理されるべき YAML ファイルが大量かつ膨大になってしまった際に，独自の型や関数・値の定義，ファイルの分割，型検査などの恩恵が得られるのはありがたい．

## おわりに

今回は煩雑な YAML 管理や，それを解決しようとする本流の解法に対するある種のアンチ・テーゼとして Dhall を紹介した．Dhall 自体は比較的新しいツールセットであり，現状広く普及しているわけでもない．実際に Kubernetes の YAML の管理をこれだけでまかないきれるのかと問われると，「試していないのでわからない」というのが正直なところである[^6]．しかしながら，安全性や分散性に気を遣いつつ，設定記述言語に必要なだけの表現力を持ちながらもチューリング完全とではないという**分を弁えた**言語というコンセプトは目新しく興味深いものであるし，その有用性が十分伺えるものである．しかも，新しいツールセットといえども，今回紹介した `dhall-to-yaml` の他に，JSON を出力する [dhall-to-json](https://github.com/dhall-lang/dhall-json)，テンプレートエンジンたる [dhall-to-text](https://github.com/dhall-lang/dhall-text)，Bash に変換する [dhall-to-bash](https://github.com/dhall-lang/dhall-bash)，Cabal ファイルを出力する [dhall-to-cabal](https://github.com/dhall-lang/dhall-to-cabal)，[Haskell バインディング](https://github.com/dhall-lang/dhall-haskell) などを備え，既に幅広いツールが提供されている．

今回は Kubernetes を例に取ったが，設定記述のために JSON や YAML を書かされるシチュエーションは世の中に溢れ返っている．とりわけ CI のタスクの記述などは，スキーマが存在し，かつプログラマティックに記述できればどれだけありがたいだろうかと常々思うものだが，Dhall はこのような状況を解決する可能性を秘めている．このような有用なツールセットが広く普及することが期待される．

記事中のコードは GitHub からどうぞ．

https://github.com/ryota-ka/dhall-k8s-example

## 脚注

[^1]: この冒頭挨拶は[「kubernetesに自分のコードがマージされるまでのフロー」](https://blog.whywrite.it/2018/06/15/how-to-submit-pr-to-kubernetes/)のオマージュです．
[^2]: 一体どれだけの実装が YAML の仕様を満たしているというのか！
[^3]: "yes" を真だとみなしたりだとか．
[^4]: 問題がある方はやはり公式の GitHub Wiki を読むとよい．
[^5]: 実際に[同名の関数が Prelude で定義されている](https://github.com/dhall-lang/Prelude/blob/master/Optional/Some)．
[^6]: 恵比寿の方のとある会社では実戦に投入しているという噂を耳にした
