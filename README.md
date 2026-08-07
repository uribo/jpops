
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jpops <img src="man/figures/logo.png" align="right" width="120px" />

(Sorry, English version of README is not availavle for now.)

<!-- badges: start -->

[![R-CMD-check](https://github.com/uribo/jpops/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/uribo/jpops/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/jpops)](https://CRAN.R-project.org/package=jpops)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

jpopsは日本国内で行われた人口に関する統計調査のデータを提供するRパッケージです。

一部のデータの取得はe-StatのAPIを経由して行います。APIを利用するためのapplication
idは各自で取得してください。

## インストール

パッケージは現在CRANには登録されていません。次のコマンドを実行してインストールをしてください。

``` r
install.packages(
   "jpops", 
   repos = c(mm = "https://uribo.r-universe.dev", getOption("repos")))
```

## 特徴

- 必要なデータを参照するための関数を提供します。
- 一部のデータの取得はe-Stat
  APIを経由して行いますが、パッケージ内でキャッシュ機能を利用できます。
  これによりデータの再取得にかかる時間を短縮できます。
  - キャッシュ先のフォルダは `rappdirs::user_cache_dir("jpops")`
    で確認できます。
- データをtidyな状態で提供します。
  - `get_jinkou()` は `gender`, `area_code`, `area`,
    `value`、`get_jinkou_age()` は `gender`, `area_code`, `area`, `age`,
    `value` の順で返します。
  - 調査時期によって異なる元データの分類軸を上記の列に統一します。これにより複数年を跨いだデータの結合と分析が容易になります。
  - 元データが表形式ソフトのファイルで提供されている場合、適切な範囲を選択した状態でデータを読み込みます。

## 使い方

``` r
library(jpops)
```

### 国勢調査

### 人口等基本集計 (2005~2020)

男女別総数

``` r
Sys.setenv("ESTAT_TOKEN") <- "{取得したapplication id}"

get_jinkou(2020, appid = Sys.getenv("ESTAT_TOKEN"))

# 2回目以降のデータ取得はキャッシュを利用して高速化できます。
# この時appid引数の入力は必須ではありません
get_jinkou(2020, cache = TRUE)

# キャッシュを利用せずAPI経由でデータを再取得するには cache = FALSE を指定します
get_jinkou(2020, appid = Sys.getenv("ESTAT_TOKEN"), cache = FALSE)
```

男女・年齢別

``` r
get_jinkou_age(2020, appid = Sys.getenv("ESTAT_TOKEN"))
```
