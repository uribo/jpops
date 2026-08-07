
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

`.area` には `"all"`、`"prefecture"`、`"municipality"`、`"ward"`
を指定できます。既定の `"all"`
は全国・都道府県・市町村・区・旧市町村など、親地域と子地域が重複して含まれるため、人口を合計してはいけません。`"prefecture"`
は47都道府県だけを返します。

`"municipality"`
は、通常の市、政令指定都市（市全体を1単位）、東京都の特別区部（23区全体を1単位）、町村を返します。個別の特別区、政令指定都市の区、2000年時点の旧市町村は除外します。このため、東京都については日常語の「市区町村」より粗い単位です。`"ward"`
は同じ範囲を基礎としつつ、政令指定都市を各区へ、東京都の特別区部を23の個別特別区へ分割します。どちらも親地域と子地域が重ならない地域集合です。

旧値 `"city"` は1リリース限りの非推奨aliasで、警告を出して
`"municipality"` と同じ結果を返します。旧 `"city"`
は親地域と子地域が混在していたため、名前だけでなく結果の意味と人口合計が変わります。旧結果を再現する専用オプションはありません。必要な場合は
`.area = "all"` の結果を利用者側で明示的に絞り込んでください。

`.area = "all"` と `"prefecture"`
の地域フィルタは地域メタ情報を使わないため、そのための `appid`
は不要です。ただし、処理済み人口キャッシュがなく、人口データ自体をe-Statから取得する場合は従来どおり
`appid` が必要です。`"municipality"`、`"ward"`、非推奨の `"city"`
は、これに加えて対象の `statsDataId`
に対応する地域メタ情報キャッシュがなければ初回だけ `appid`
が必要です。地域メタ情報と人口データがともにキャッシュされた後は
`appid = NULL` で利用できますが、`cache = FALSE` では毎回 `appid`
が必要です。

``` r
Sys.setenv(ESTAT_TOKEN = "{取得したapplication id}")

get_jinkou(2020, appid = Sys.getenv("ESTAT_TOKEN"))

# 親子重複のない自治体単位を取得します。
get_jinkou(
  2020,
  appid = Sys.getenv("ESTAT_TOKEN"),
  .area = "municipality"
)

# 政令指定都市と東京都特別区部を個別の区へ分割します。
get_jinkou(
  2020,
  appid = Sys.getenv("ESTAT_TOKEN"),
  .area = "ward"
)

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
