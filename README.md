
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jpops <img src="man/figures/logo.png" align="right" width="120px" />

(Sorry, English version of README is not availavle for now.)

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/jpops)](https://CRAN.R-project.org/package=jpops)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

jpopsは日本国内で行われた人口に関する統計調査のデータを提供するRパッケージです。

データの取得はe-StatのAPIを経由して行います。APIを利用するためのapplication
idは各自で取得してください。

## Installation

パッケージは現在CRANには登録されていません。次のコマンドを実行してインストールをしてください。

``` r
if (!requireNamespace("remotes"))
   install.packages("remotes")
remotes::install_github("uribo/jpops")
```

## 特徴

-   必要なデータを参照するための関数を提供します。
-   データの取得はe-Stat
    APIを経由して行いますが、パッケージ内でキャッシュ機能を利用できます。
    これによりデータの再取得にかかる時間を短縮できます。
    -   キャッシュ先のフォルダは `rappdirs::user_cache_dir("jpops")`
        で確認できます。
-   データをtidyな状態で提供します。

## Example

``` r
library(jpops)
```

### 国勢調査

### 人口等基本集計 (2020\~2000)

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
