
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jpops <img src="man/figures/logo.png" align="right" width="120px" />

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

## Example

``` r
library(jpops)
```

### 国勢調査

人口等基本集計

``` r
Sys.setenv("ESTAT_TOKEN") <- "{取得したapplication id}"

get_jinkou(2020, appid = Sys.getenv("ESTAT_TOKEN"))
```
