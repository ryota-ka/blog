---
keywords:
  - Terraform
---

# Terraformのmoduleで2重ループしたい — GitHubのissue labelsを例に

## 実現したいこと

[Terraform](https://www.terraform.io/)の[GitHub provider](https://www.terraform.io/docs/providers/github/index.html)を用いて，issuesおよびpull requestsのラベルを管理したい．その際に，いくつかのラベルをひとまとめにしておき，複数のリポジトリが同じラベル群を持つようにしたい．また，[moduleはネストすることもできる](https://www.terraform.io/docs/modules/create.html#nested-modules)が，複雑になるので避けたい．

---

## 環境

Terraform v0.9.11

## 方針

[`github_issue_label`](https://www.terraform.io/docs/providers/github/r/issue_label.html)をベースにモジュールを作成する．管理したいリポジトリの一覧を*list*で，ラベルの一覧をラベル名からカラーコードへの*map*で，それぞれモジュールに渡すことにする．

```hcl
variable "repositories" {
  type        = "list"
  default     = []
}

variable "labels" {
  type        = "map"
  default     = {}
}
```

[`count` _meta-parameter_](https://www.terraform.io/docs/configuration/resources.html#count)を用いれば複数のリソースを作成することができるので，これをうまく使う．今回のケースでは，リポジトリの集合とラベルの集合のデカルト積の上でのイテレーションが行えればよい．有限集合同士のデカルト積の濃度は，それぞれの集合の濃度の積になるので，`count = "${length(var.repositories) * length(var.labels)}"`としてやればうまくいく．あとは除算とか剰余とかを使って適当に辻褄を合わせる．

```hcl
variable "repositories" {
  type        = "list"
  default     = []
}

variable "labels" {
  type        = "map"
  default     = {}
}

resource "github_issue_label" "labels" {
  count      = "${length(var.repositories) * length(var.labels)}"
  repository = "${element((var.repositories), count.index / length(var.labels))}"
  name       = "${element(keys(var.labels), count.index % length(var.labels))}"
  color      = "${element(values(var.labels), count.index % length(var.labels))}"
}
```

以下のような形で使える．

```
module "labels" {
  source = "./path/to/labels"

  repositories = [
    "repo-a",
    "repo-b",
    "repo-c"
  ]

  labels {
    "Label A" = "012345"
    "Label B" = "6789ab"
    "Label C" = "cdef01"
    "日本語も使えるよ"       = "234567"
  }
}
```

日本語を使うと`$ terraform fmt`による整形結果が気持ち悪い．

`$ terraform plan`を実行すれば，期待通りすべてのリポジトリに同じラベル群が適用されるであろうことを伝えてくれるはずだ．
